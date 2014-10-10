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

#ifndef __BASE_RENDERER_H__
#define __BASE_RENDERER_H__

#include "base.h"
#include "base_image.h"
#include "base_window.h"
#include "vm_thread.h"

class BaseRenderer
{
public:
    BaseRenderer(void) {};
    virtual ~BaseRenderer(void) {};

    virtual Status Init(RendererContext *pContext) = 0;

    virtual Status Close() = 0;

    virtual Status InitForThread() = 0;

    virtual void   SetViewport(Rect rect) = 0;

    virtual Rect   ScaleViewRect(Rect rect) = 0;

    virtual Status RenderFrame(Image *pFrame, bool bDelayed = false) = 0;

    virtual Status Flush() = 0;
};

#ifdef ENABLE_RENDERING

#if defined _WIN32
#include <GL/gl.h>
#else
#include <GL/glx.h>
#endif

class RendererOpenGL : public BaseRenderer
{
public:
    RendererOpenGL(void);
    virtual ~RendererOpenGL(void);

    Status Init(RendererContext *pContext);

    Status Close();

    Status InitForThread();

    void   SetViewport(Rect rect);

    Rect   ScaleViewRect(Rect rect);

    Status RenderFrame(Image *pFrame, bool bDelayed = false);

    Status Flush();

protected:
#if defined _WIN32
    HDC         m_wdc;
    HGLRC       m_glContext;
    HWND        m_hWnd;
#else
    GLXContext   m_glContext;
    Display*     m_display;
    Window       m_window;
    XVisualInfo* m_pvisinfo;
    unsigned int m_iScreen;
#endif

    Image        m_template;
    Rect         m_rectPrev;
    bool         m_bInitialized;
    bool         m_bKeepAspect;

    Image   m_texData; // bufer for the texture
    GLuint  m_texture;
    float   m_fTexWidth;
    float   m_fTexHeight;
};

#endif

/*
// Window helpers
*/
#define WF_FIT_TO_IMAGE 0x0001

struct WndDesc
{
    WndDesc()
    {
        pWndClass = NULL;
        pRenClass = NULL;

        iWndStyle = 0;
        iFlags    = 0;

        bFirstFrame = true;

        vm_thread_set_invalid(&wndProcThread);
        vm_event_set_invalid(&wndInited);
        bKillWndThread = false;
    }

    WindowBase   *pWndClass;
    BaseRenderer *pRenClass;

    DString   sWndName;
    int       iWndStyle;
    vm_thread wndProcThread;
    vm_event  wndInited;
    bool      bKillWndThread;

    bool bFirstFrame;

    unsigned int  iFlags;
};

WndDesc* WindowNew(const char* sName, unsigned int iFlags = 0);
void     WindowRelease(WndDesc *pDesc);
Status   WindowDrawImage(WndDesc *pDesc, Image *pImage, bool bDelayed = false);
Status   WindowSwapBuffers(WndDesc *pDesc);
bool     WindowIsClosed(WndDesc *pDesc);
bool     WindowIsInvalidated(WndDesc *pDesc);
int      WindowCheckKey(WndDesc *pDesc);
int      WindowWaitKey(WndDesc *pDesc);
Status   WindowWaitQuit(WndDesc *pDesc);

#endif

#endif      // __MIC__
