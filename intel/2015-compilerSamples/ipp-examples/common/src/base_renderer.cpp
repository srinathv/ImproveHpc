/*******************************************************************************
** Copyright(C) 2003-2014 Intel Corporation. All Rights Reserved.
**                                                                             
** The source code, information and material ("Material") contained herein is 
** owned by Intel Corporation or its suppliers or licensors, and title 
** to such Material remains with Intel Corporation or its suppliers or 
** licensors. The Material contains proprietary information of Intel or 
** its suppliers and licensors. The Material is protected by worldwide 
** copyright laws and treaty provisions. No part of the Material may be used, 
** copied, reproduced, modified, published, uploaded, posted, transmitted, 
** distributed or disclosed in any way without Intel's prior express written 
** permission. No license under any patent, copyright or other intellectual 
** property rights in the Material is granted to or conferred upon you, 
** either expressly, by implication, inducement, estoppel or otherwise. 
** Any license under such intellectual property rights must be express and 
** approved by Intel in writing. Unless otherwise agreed by Intel in writing, 
** you may not remove or alter this notice or any other notice embedded in 
** Materials by Intel or Intel's suppliers or licensors in any way.
*/

#if !defined(__MIC__)

#include "base_renderer.h"

#ifdef ENABLE_RENDERING

static unsigned int SetColorFormat(ImageColor format)
{
    switch(format)
    {
    case IC_GRAY: return GL_LUMINANCE;
    case IC_BGR:  return GL_BGR_EXT;
    case IC_BGRA: return GL_BGRA_EXT;
    case IC_RGB:  return GL_RGB;
    case IC_RGBA: return GL_RGBA;
    default:   return 0;
    }
    return 0;
}


RendererOpenGL::RendererOpenGL(void)
{
    m_bInitialized   = false;

#if defined _WIN32
    m_hWnd      = 0;
    m_wdc       = 0;
    m_glContext = 0;
#else
    m_glContext = 0;
#endif

    m_rectPrev.x = m_rectPrev.y = 0;
    m_rectPrev.width = m_rectPrev.height = 0;
}

RendererOpenGL::~RendererOpenGL(void)
{
    Close();
}

Status RendererOpenGL::Init(RendererContext *pContext)
{
    if(!pContext)
        return STS_ERR_NULL_PTR;

    Close();

    m_bKeepAspect = true;

#if defined _WIN32
    m_hWnd = pContext->m_hWnd;

    PIXELFORMATDESCRIPTOR pfd = {
        sizeof(PIXELFORMATDESCRIPTOR),  1, PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER,
        PFD_TYPE_RGBA, 24, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, PFD_MAIN_PLANE, 0, 0, 0, 0
    };

    m_wdc = GetDC(m_hWnd);
    if(NULL == m_wdc)
        return STS_ERR_FAILED;

    if(!SetPixelFormat(m_wdc, ChoosePixelFormat(m_wdc, &pfd), &pfd))
        return STS_ERR_FAILED;
#else
    m_display        = pContext->m_pDisplay;
    m_window         = pContext->m_window;
    m_iScreen        = pContext->m_iScreen;
    m_pvisinfo       = pContext->m_pVisualInfo;
#endif

    if(InitForThread() != STS_OK)
        return STS_ERR_FAILED;

    m_bInitialized = true;

    return STS_OK;
}

Status RendererOpenGL::Close(void)
{
#if defined _WIN32
    if(m_glContext)
    {
        wglDeleteContext(m_glContext);
        m_glContext = 0;
    }
    if(m_wdc && m_hWnd)
    {
        ReleaseDC(m_hWnd, m_wdc);
        m_wdc = 0;
    }
#else
    if(m_glContext)
    {
        glXDestroyContext(m_display, m_glContext);
        m_glContext = 0;
    }
#endif

    m_texData.Release();

    return STS_OK;
}

Status RendererOpenGL::InitForThread()
{
#if defined _WIN32
    m_glContext = wglCreateContext(m_wdc); // create rendering context
    if(NULL == m_glContext)
        return STS_ERR_FAILED;

    if(!wglMakeCurrent(m_wdc, m_glContext)) // set it as current
        return STS_ERR_FAILED;
#else
    if (NULL == (m_glContext = glXCreateContext( m_display, m_pvisinfo, NULL, true )))
        return STS_ERR_FAILED;

    if (!glXMakeCurrent(m_display, m_window, m_glContext))
        return STS_ERR_FAILED;
#endif

    // OpenGL context already tied to output window
    // to disable all slow GL components
    // it is not mandatory to disable all if we have accelerated card
    glClearColor(.0f, .0f, .0f, .0f);
    glClearDepth(1.0);
    glDepthFunc(GL_NEVER);

    // disable slow GL extensions
    glDisable(GL_DEPTH_TEST); glDisable(GL_ALPHA_TEST);   glDisable(GL_BLEND);
    glDisable(GL_DITHER);     glDisable(GL_FOG);          glDisable(GL_STENCIL_TEST);
    glDisable(GL_LIGHTING);   glDisable(GL_LOGIC_OP);     glDisable(GL_TEXTURE_1D);

    glPixelTransferi(GL_MAP_COLOR, GL_FALSE);
    glPixelTransferi(GL_RED_SCALE,   1);  glPixelTransferi(GL_RED_BIAS,   0);
    glPixelTransferi(GL_GREEN_SCALE, 1);  glPixelTransferi(GL_GREEN_BIAS, 0);
    glPixelTransferi(GL_BLUE_SCALE,  1);  glPixelTransferi(GL_BLUE_BIAS,  0);
    glPixelTransferi(GL_ALPHA_SCALE, 1);  glPixelTransferi(GL_ALPHA_BIAS, 0);

    glEnable(GL_TEXTURE_2D);
    glGenTextures(1, &m_texture);
    glBindTexture(GL_TEXTURE_2D, m_texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

#if defined _WIN32
    SwapBuffers(m_wdc);
#else
    glXSwapBuffers(m_display, m_window);
#endif

    return STS_OK;
}

Rect RendererOpenGL::ScaleViewRect(Rect rect)
{
#if defined _WIN32
    ::RECT wndRect;
    GetClientRect(m_hWnd, &wndRect);
    int iWndWidth  = wndRect.right;
    int iWndHeight = wndRect.bottom;
#else
    XWindowAttributes wattc = {0};
    XGetWindowAttributes(m_display, m_window, &wattc);
    int iWndWidth  = wattc.width;
    int iWndHeight = wattc.height;
#endif

    if(m_bKeepAspect)
    {
        float fScale = MIN((float)iWndWidth/rect.width, (float)iWndHeight/rect.height);
        rect.width  = (unsigned int)(fScale*rect.width);
        rect.height = (unsigned int)(fScale*rect.height);
    }

    rect.x = (rect.x >= 0)?(rect.x):((iWndWidth - rect.width)/2);
    rect.y = (rect.y >= 0)?(rect.y):((iWndHeight - rect.height)/2);

    return rect;
}

void RendererOpenGL::SetViewport(Rect rect)
{
    if(m_rectPrev.x != rect.x || m_rectPrev.y != rect.y || m_rectPrev.width != rect.width || m_rectPrev.height != rect.height)
    {
        m_rectPrev = rect;
        glViewport(m_rectPrev.x, m_rectPrev.y, m_rectPrev.width, m_rectPrev.height);
    }
}

Status RendererOpenGL::RenderFrame(Image *pFrame, bool bDelayed)
{
    Status status = STS_OK;
    GLenum glSts;
    bool   bTexChange   = false;
    bool   bUseOldFrame = false;

    if(!pFrame)
    {
        if(m_texData.m_pPointer)
        {
            pFrame = &m_template;
            bUseOldFrame = true;
        }
        else
            return STS_ERR_NULL_PTR;
    }

    int iRenderFormat = SetColorFormat(pFrame->m_color);

    // reinit buffers if window size has been changed
    if(m_template != *pFrame)
    {
        if(!bUseOldFrame)
        {
            if(m_texData.m_iWidth < pFrame->m_iWidth || m_texData.m_iHeight < pFrame->m_iHeight)
            {
                m_texData.m_iWidth  = 1;
                m_texData.m_iHeight = 1;

                do
                {
                    if (m_texData.m_iWidth < pFrame->m_iWidth)
                        m_texData.m_iWidth <<= 1;
                    if (m_texData.m_iHeight < pFrame->m_iHeight)
                        m_texData.m_iHeight <<= 1;
                } while((m_texData.m_iWidth < pFrame->m_iWidth) || (m_texData.m_iHeight < pFrame->m_iHeight));

                bTexChange = true;
            }

            if(bTexChange || m_template.m_color != pFrame->m_color || m_template.m_iSamples != pFrame->m_iSamples)
            {
                status = m_texData.Alloc(m_texData.m_iWidth, m_texData.m_iHeight, pFrame->m_iSamples);
                if(STS_OK != status)
                    return STS_ERR_FAILED;
            }

            m_fTexWidth = (float)((double)pFrame->m_iWidth/(double)m_texData.m_iWidth);
            m_fTexHeight = (float)((double)pFrame->m_iHeight/(double)m_texData.m_iHeight);
        }

        m_template = *pFrame;
    }

    // resize to output pixel buffer
    if(m_rectPrev.width != 0 && m_rectPrev.height != 0)
    {
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        if(!bUseOldFrame)
        {
            status = pFrame->CopyTo(&m_texData);
            if(STS_OK != status)
                return STS_ERR_FAILED;

            glTexImage2D(GL_TEXTURE_2D, 0, pFrame->m_iSamples, m_texData.m_iWidth, m_texData.m_iHeight, 0, iRenderFormat, GL_UNSIGNED_BYTE, m_texData.m_pPointer);

            glSts = glGetError();
            if(glSts != GL_NO_ERROR)
                return STS_ERR_FAILED;
        }

        glBegin(GL_POLYGON);
        glColor3ub(255, 255, 255);
        glTexCoord2f(0.,          0.);           glVertex2f(-1.0,  1.0);
        glTexCoord2f(m_fTexWidth, 0.);           glVertex2f( 1.0,  1.0);
        glTexCoord2f(m_fTexWidth, m_fTexHeight); glVertex2f( 1.0, -1.0);
        glTexCoord2f(0.,          m_fTexHeight); glVertex2f(-1.0, -1.0);
        glEnd();

        glSts = glGetError();
        if(glSts != GL_NO_ERROR)
            return STS_ERR_FAILED;

        if(!bDelayed)
        {
            status = Flush();
            CHECK_STATUS_PRINT_RS(status, "RendererOpenGL::Flush()", GetBaseStatusString(status));
        }
    }

    return status;
}

Status RendererOpenGL::Flush()
{
    GLenum glSts;

    glFlush();
    glSts = glGetError();
    if(glSts != GL_NO_ERROR)
        return STS_ERR_FAILED;

#if defined _WIN32
    if(!SwapBuffers(m_wdc))   // to draw on physical screen
        return STS_ERR_FAILED;
#else
    glXSwapBuffers(m_display, m_window);
#endif

    return STS_OK;
}

#endif

/*
// Window helpers
*/
void keyCall(int iKeyCode, int iCount, unsigned int iFlags, void *pParams)
{
    WndDesc *pDesc = (WndDesc*)pParams;

    if(!pDesc)
        return;

    if((iFlags & KF_MESSAGE_UP) || !(iFlags & KF_STATE_CHANGE))
        return;

    if(iKeyCode == KK_ESCAPE)
    {
        pDesc->pWndClass->m_feedback.bDestroyMessage = true;
    }
    else if(iKeyCode == KK_F)
    {
        pDesc->pWndClass->WindowSwitchFullscreen();
        pDesc->pWndClass->Invalidate();
    }
/*    else if(iKeyCode == KK_SPACE)
    {
        //pDesc->pWndClass->Invalidate();
    }*/

    return;
}

unsigned int VM_THREAD_CALLCONVENTION wndProcThread(void *pParams)
{
    WndDesc *pDesc = (WndDesc*)pParams;

    if(!pDesc->pWndClass->WindowCreate(pDesc->sWndName, pDesc->iWndStyle))
    {
        delete pDesc->pRenClass;
        delete pDesc->pWndClass;
        delete pDesc;
        return 1;
    }

    pDesc->pWndClass->WindowResize(200, 200);
    pDesc->pWndClass->WindowShow();

    pDesc->pWndClass->AddKeyboardCallback(keyCall, pDesc);

    vm_event_signal(&pDesc->wndInited);

    while(!pDesc->bKillWndThread)
    {
        pDesc->pWndClass->CheckMessages();
        vm_time_sleep(10);
    }

    return 0;
}

WndDesc* WindowNew(const char* sName, unsigned int iFlags)
{
#ifdef ENABLE_RENDERING
    if(!sName)
        return 0;

    WndDesc *pDesc    = new WndDesc();
    pDesc->pRenClass  = 0;
    pDesc->pWndClass  = 0;
    pDesc->iFlags     = iFlags;
    int iStyle        = WSTYLE_NORMAL;

#if defined _WIN32
    pDesc->pWndClass = new WindowWin();
#elif defined UNIX
    pDesc->pWndClass = new WindowX();
#endif
    if(!pDesc->pWndClass)
    {
        delete pDesc;
        return 0;
    }

    pDesc->pRenClass = new RendererOpenGL();
    if(!pDesc->pRenClass)
    {
        delete pDesc->pWndClass;
        delete pDesc;
        return 0;
    }

    pDesc->pWndClass->m_feedback.keyCall = 0;

    // create window
    if(iFlags & WF_FIT_TO_IMAGE)
        iStyle = WSTYLE_FIXED_SIZE;

    pDesc->sWndName  = sName;
    pDesc->iWndStyle = iStyle;

    vm_event_init(&pDesc->wndInited, 0, 0);
    vm_thread_create(&pDesc->wndProcThread, &wndProcThread, pDesc);
    vm_event_wait(&pDesc->wndInited);

    RendererContext context;
    pDesc->pWndClass->GetWindowContext(&context);
    pDesc->pRenClass->Init(&context);

    return pDesc;
#else
    return 0;
#endif
}

void WindowRelease(WndDesc *pDesc)
{
    if(!pDesc)
        return;

    if(pDesc->pRenClass)
    {
        pDesc->pRenClass->Close();
        delete pDesc->pRenClass;
    }

    pDesc->bKillWndThread = true;
    vm_thread_wait(&pDesc->wndProcThread);
    vm_thread_close(&pDesc->wndProcThread);
    vm_event_destroy(&pDesc->wndInited);

    if(pDesc->pWndClass)
    {
        pDesc->pWndClass->WindowClose();
        delete pDesc->pWndClass;
    }

    delete pDesc;
}

Status WindowDrawImage(WndDesc *pDesc, Image *pImage, bool bDelayed)
{
    Status status;

    if(!pImage || !pDesc || !pDesc->pRenClass || !pDesc->pWndClass)
        return STS_ERR_NULL_PTR;

    Rect rect = {-1, -1, 0, 0};

    if(pImage)
    {
        if(pDesc->bFirstFrame || (pDesc->iFlags & WF_FIT_TO_IMAGE))
        {
            pDesc->pWndClass->WindowResize(pImage->m_iWidth, pImage->m_iHeight);
            pDesc->bFirstFrame = false;

            status = WindowSwapBuffers(pDesc);
            CHECK_STATUS_PRINT_RS(status, "WindowSwapBuffers()", GetBaseStatusString(status));
        }
    }

    rect.width  = pImage->m_iWidth;
    rect.height = pImage->m_iHeight;

    rect = pDesc->pRenClass->ScaleViewRect(rect);
    pDesc->pRenClass->SetViewport(rect);
    status = pDesc->pRenClass->RenderFrame(pImage, bDelayed);

    return status;
}

Status WindowSwapBuffers(WndDesc *pDesc)
{
    if(!pDesc || !pDesc->pRenClass || !pDesc->pWndClass)
        return STS_ERR_NULL_PTR;

    return pDesc->pRenClass->Flush();
}

bool WindowIsClosed(WndDesc *pDesc)
{
    if(!pDesc || !pDesc->pWndClass || pDesc->pWndClass->m_feedback.bDestroyMessage)
        return true;

    return false;
}

bool WindowIsInvalidated(WndDesc *pDesc)
{
    if(pDesc->pWndClass->m_feedback.iRepaint)
    {
        pDesc->pWndClass->m_feedback.iRepaint = 0;
        return true;
    }
    return false;
}

int WindowCheckKey(WndDesc *pDesc)
{
    if(!pDesc || !pDesc->pWndClass)
        return STS_ERR_NULL_PTR;

    int iKey = pDesc->pWndClass->m_feedback.iLastKey;
    pDesc->pWndClass->m_feedback.iLastKey = -1;

    return iKey;
}

int WindowWaitKey(WndDesc *pDesc)
{
    if(!pDesc || !pDesc->pWndClass)
        return STS_ERR_NULL_PTR;

    while(!pDesc->pWndClass->m_feedback.bDestroyMessage && pDesc->pWndClass->m_feedback.iLastKey == -1)
        vm_time_sleep(10);

    int iKey = pDesc->pWndClass->m_feedback.iLastKey;
    pDesc->pWndClass->m_feedback.iLastKey = -1;

    return iKey;
}

Status WindowWaitQuit(WndDesc *pDesc)
{
    if(!pDesc || !pDesc->pWndClass)
        return STS_ERR_NULL_PTR;

    while(!pDesc->pWndClass->m_feedback.bDestroyMessage)
        vm_time_sleep(10);

    // don't close we need to release renderer first
    pDesc->pWndClass->WindowHide();

    return STS_OK;
}

#endif
