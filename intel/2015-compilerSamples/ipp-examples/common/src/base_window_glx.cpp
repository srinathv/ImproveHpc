/*******************************************************************************
** Copyright(C) 2010-2014 Intel Corporation. All Rights Reserved.
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

#if defined UNIX

#include "base_window.h"

#ifdef ENABLE_RENDERING
#include <X11/keysym.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <GL/glx.h>

struct MotifHints
{
    unsigned long   flags;
    unsigned long   functions;
    unsigned long   decorations;
    long            inputMode;
    unsigned long   status;
};

WindowX::WindowX()
{
    m_display        = NULL;
    m_window         = 0;
    m_pvisinfo       = NULL;
    m_iScreen        = 0;
    m_wndRect.width  = m_wndRect.height = 100;
    m_iPrevPressTime = 0;

    XInitThreads();
}

WindowX::~WindowX()
{
    WindowClose();
}

bool WindowX::WindowCreate(const char* cAppName, unsigned int iStyle)
{
    XSetWindowAttributes attributes;
    int attr[] = { GLX_RGBA, GLX_DOUBLEBUFFER, GLX_RED_SIZE, 8, GLX_GREEN_SIZE, 8, GLX_BLUE_SIZE, 8, GLX_ALPHA_SIZE, 0, None };

    m_display = XOpenDisplay(getenv("DISPLAY"));
    if(NULL == m_display)
        return false;

    m_iScreen = DefaultScreen(m_display);

    if (NULL == (m_pvisinfo = glXChooseVisual( m_display, m_iScreen, attr )))
        return false;

    // initial color map for TrueColor and Empty table for Direct Color
    attributes.colormap     = XCreateColormap( m_display, RootWindow(m_display, m_iScreen), m_pvisinfo->visual, AllocNone);
    attributes.event_mask   = ExposureMask | KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask | KeymapStateMask;
    unsigned long valuemask = CWColormap | CWEventMask;

    if (0 == (m_window = XCreateWindow(m_display, RootWindow(m_display, m_iScreen), m_wndRect.x, m_wndRect.y, m_wndRect.width, m_wndRect.height, 2,
        m_pvisinfo->depth, InputOutput, m_pvisinfo->visual, valuemask, &attributes)))
        return false;

    XStoreName(m_display, m_window, cAppName);

    // init interface close message
    m_wmDelete = XInternAtom(m_display, "WM_DELETE_WINDOW", true);
    XSetWMProtocols(m_display, m_window, &m_wmDelete, 1);

    XFlush(m_display);

    m_bWndCreated = true;
    return true;
}

void WindowX::WindowClose()
{
    if(!m_bWndCreated)
        return;

    if (NULL != m_pvisinfo)
    {
        XFree(m_pvisinfo);
        m_pvisinfo = NULL;
    }
    XDestroyWindow(m_display, m_window);
    XCloseDisplay(m_display);

    m_bWndCreated = false;
}

void WindowX::WindowSetRect(WndRect wndRect)
{
    if(!m_bWndCreated)
        return;

    m_wndRect = wndRect;
    XMoveResizeWindow(m_display, m_window, m_wndRect.x, m_wndRect.y, m_wndRect.width, m_wndRect.height);
}

void WindowX::WindowResize(unsigned int iWidth, unsigned int iHeight)
{
    if(!m_bWndCreated)
        return;

    m_wndRect.width  = iWidth;
    m_wndRect.height = iHeight;
    XMoveResizeWindow(m_display, m_window, m_wndRect.x, m_wndRect.y, m_wndRect.width, m_wndRect.height);
    XFlush(m_display);
}

void WindowX::WindowSetFullscreen(bool bFullscreen)
{
#ifndef __APPLE__ // X11 fullscreen event doesn't work properly on Mac
    XEvent event;

    if(!m_bWndCreated)
        return;

    if(bFullscreen == m_bFullscreen)
        return;

    event.type                 = ClientMessage;
    event.xclient.type         = ClientMessage;
    event.xclient.display      = m_display;
    event.xclient.window       = m_window;
    event.xclient.serial       = 0;
    event.xclient.send_event   = True;
    event.xclient.format       = 32;
    event.xclient.message_type = XInternAtom(m_display, "_NET_WM_STATE", true);

    event.xclient.data.l[1]    = XInternAtom(m_display, "_NET_WM_STATE_FULLSCREEN", true);
    event.xclient.data.l[2]    = 0;
    event.xclient.data.l[3]    = 0;

    if(bFullscreen)
    {
        event.xclient.data.l[0] = 1;
        m_bFullscreen = true;
    }
    else
    {
        event.xclient.data.l[0] = 0;
        m_bFullscreen = false;
    }

    XSendEvent(m_display, RootWindow(m_display, m_iScreen), False, SubstructureRedirectMask | SubstructureNotifyMask, &event);

    XFlush(m_display);
#endif
}

void WindowX::WindowShow()
{
    if(!m_bWndCreated)
        return;

    XMapRaised(m_display, m_window);
    XFlush(m_display);
}

void WindowX::WindowHide()
{
    if(!m_bWndCreated)
        return;

    XUnmapWindow(m_display, m_window);
    XFlush(m_display);
}

void WindowX::WindowGetSize(unsigned int &iWidth, unsigned int &iHeight)
{
    if(!m_bWndCreated)
        return;

    XWindowAttributes wattc = {0};
    XGetWindowAttributes(m_display, m_window, &wattc);
    iWidth  = wattc.width;
    iHeight = wattc.height;
}

void WindowX::GetWindowContext(RendererContext *pContext)
{
    if(!m_bWndCreated)
        return;

    if(!pContext)
        return;

    pContext->m_pDisplay     = m_display;
    pContext->m_window       = m_window;
    pContext->m_iScreen      = m_iScreen;
    pContext->m_pVisualInfo  = m_pvisinfo;
}

void WindowX::WindowProc(XEvent &event)
{
    switch(event.type)
    {
    case ClientMessage:
        if((unsigned long)event.xclient.data.l[0] == m_wmDelete)
            m_feedback.bDestroyMessage = true;
        break;

    case Expose:
        m_feedback.iRepaint++;
        break;

    case KeyPress:
    {
        m_feedback.iLastKey = (int)XLookupKeysym(&event.xkey, 0);
        if(m_feedback.keyCall)
        {
            unsigned int iFlags = 0;
            unsigned int iCount = 0;

            if(m_feedback.iLastState != KeyPress)
                iFlags |= KF_STATE_CHANGE;
            m_feedback.iLastState = KeyPress;

            m_feedback.keyCall(m_feedback.iLastKey, iCount, iFlags, m_feedback.keyCallParams);
        }
        break;
    }

    case KeyRelease:
    {
        if(m_feedback.keyCall)
        {
            unsigned int iFlags = KF_MESSAGE_UP;
            unsigned int iCount = 0;

            if(m_feedback.iLastState != KeyRelease)
                iFlags |= KF_STATE_CHANGE;
            m_feedback.iLastState = KeyRelease;

            m_feedback.keyCall((int)XLookupKeysym(&event.xkey, 0), iCount, iFlags, m_feedback.keyCallParams);
        }
        break;
    }

    case MappingNotify:
        XRefreshKeyboardMapping(&event.xmapping);
        break;
    }
}

void WindowX::WaitMessages()
{
    XEvent event;

    if(!m_bWndCreated)
        return;

    XNextEvent(m_display, &event);
    WindowProc(event);
}

void WindowX::CheckMessages()
{
    XEvent event;

    if(!m_bWndCreated)
        return;

    while(XEventsQueued(m_display, QueuedAfterFlush))
    {
        XNextEvent(m_display, &event);
        WindowProc(event);
    }
}

void WindowX::Invalidate()
{
    XEvent event;
    Status status;

    if(!m_bWndCreated)
        return;

    unsigned int iWidth, iHeight;
    WindowGetSize(iWidth, iHeight);

    event.type                 = Expose;
    event.xexpose.type         = Expose;
    event.xexpose.display      = m_display;
    event.xexpose.window       = m_window;
    event.xexpose.serial       = 0;
    event.xexpose.send_event   = True;
    event.xexpose.count        = 0;
    event.xexpose.x            = 0;
    event.xexpose.y            = 0;
    event.xexpose.width        = iWidth;
    event.xexpose.height       = iHeight;

    status = XSendEvent(m_display, m_window, False, ExposureMask, &event);

    XFlush(m_display);
}

void WindowX::AddKeyboardCallback(KeyboardCallback keyCall, void* pParams)
{
    m_feedback.keyCall = keyCall;
    m_feedback.keyCallParams = pParams;
}

#endif

#endif
