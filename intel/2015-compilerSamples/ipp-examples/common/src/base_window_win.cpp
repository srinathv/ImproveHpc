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

#if defined _WIN32

// media keys override for VS2005
#if _MSC_VER <= 1400 && !defined(_WIN32_WINNT)
    #define _WIN32_WINNT 0x0500
#endif

#include <windows.h>
#include "base_window.h"

#ifdef ENABLE_RENDERING
LRESULT CALLBACK WindowProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    WndFeedback *pFeedback = (WndFeedback*)GetWindowLongPtr(hWnd, GWLP_USERDATA);
    if(!pFeedback)
        return DefWindowProc(hWnd, msg, wParam, lParam);

    switch(msg)
    {
    case WM_DESTROY:
        pFeedback->bDestroyMessage = true;
        break;

    case WM_SYSCOMMAND:
        if(wParam == SC_CLOSE) // does not allow window to close by itself
        {
            pFeedback->bDestroyMessage = true;
            return 0;
        }
        break;

    case WM_KEYDOWN:
        pFeedback->iLastKey = (int)wParam;
        if(pFeedback->keyCall)
        {
            unsigned int iFlags = 0;
            unsigned int iCount = (lParam & 0xFFFF);

            if(lParam & 0x100000)
                iFlags |= KF_EXT_KEY;

            if(!(lParam & 0x40000000))
                iFlags |= KF_STATE_CHANGE;

            pFeedback->keyCall((int)wParam, iCount, iFlags, pFeedback->keyCallParams);
        }
        return 0;

    case WM_KEYUP:
        if(pFeedback->keyCall)
        {
            unsigned int iFlags = KF_MESSAGE_UP;
            unsigned int iCount = (lParam & 0xFFFF);

            if(lParam & 0x100000)
                iFlags |= KF_EXT_KEY;

            if(!(lParam & 0x40000000))
                iFlags |= KF_STATE_CHANGE;

            pFeedback->keyCall((int)wParam, iCount, iFlags, pFeedback->keyCallParams);
        }
        return 0;

    case WM_PAINT:
        pFeedback->iRepaint++;
        break;
    }

    return DefWindowProc(hWnd, msg, wParam, lParam);
}

WindowWin::WindowWin()
{
    m_hWnd      = NULL;
    m_pMenu     = NULL;
    m_wndRect.x = m_wndRect.y = CW_USEDEFAULT;
}

WindowWin::~WindowWin()
{
    WindowClose();
}

bool WindowWin::WindowCreate(const char* cAppName, unsigned int iStyle)
{
    char      *cClassName = "IPPSampleWinClass";
    WNDCLASSEX wndClass   = {0};
    LONG       lWnd;

    m_iStyle = iStyle;

    wndClass.cbSize         = sizeof(WNDCLASSEX);
    wndClass.style          = CS_DBLCLKS | CS_OWNDC | CS_HREDRAW | CS_VREDRAW;
    wndClass.lpfnWndProc    = WindowProc;
    wndClass.cbClsExtra     = 0;
    wndClass.cbWndExtra     = 0;
    wndClass.hInstance      = NULL;
    wndClass.hIcon          = NULL;
    wndClass.hCursor        = LoadCursor(0, IDC_ARROW);
    wndClass.hbrBackground  = (HBRUSH)GetStockObject(BLACK_BRUSH);
    wndClass.lpszMenuName   = NULL;
    wndClass.lpszClassName  = cClassName;
    wndClass.hIconSm        = NULL;

    if(!RegisterClassEx(&wndClass))
        return false;

    switch(m_iStyle)
    {
    case WSTYLE_BORDERLESS:
        lWnd = WS_POPUP;
        break;
    case WSTYLE_FIXED_SIZE:
        lWnd = WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX;
        break;
    case WSTYLE_NORMAL:
        lWnd = WS_OVERLAPPEDWINDOW;
        break;
    default:
        return false;
    }

    m_hWnd = CreateWindowEx(0, cClassName, cAppName, lWnd, m_wndRect.x, m_wndRect.y, m_wndRect.width, m_wndRect.height, NULL, NULL, NULL, NULL);
    if(m_hWnd == NULL)
        return false;

    SetWindowLongPtr(m_hWnd, GWLP_USERDATA, (LONG_PTR)&m_feedback);

    m_bWndCreated = true;
    return true;
}

void WindowWin::WindowClose()
{
    if(!m_bWndCreated)
        return;

    DestroyWindow(m_hWnd);
    m_bWndCreated = false;
    m_hWnd        = NULL;
}

void WindowWin::WindowSetRect(WndRect wndRect)
{
    if(!m_bWndCreated)
        return;

    m_wndRect = wndRect;
    MoveWindow(m_hWnd, m_wndRect.x, m_wndRect.y, m_wndRect.width, m_wndRect.height, 0);
}

void WindowWin::WindowResize(unsigned int iWidth, unsigned int iHeight)
{
    if(!m_bWndCreated)
        return;

    if(m_bFullscreen)
        return;

    ::RECT workArea;
    WINDOWINFO info;
    unsigned int iDescWidth;
    unsigned int iDescHeight;
    info.cbSize = sizeof(WINDOWINFO);
    GetWindowInfo(m_hWnd, &info);

    // Get work area (without taskbar and other toolbars)
    SystemParametersInfo(SPI_GETWORKAREA, 0, &workArea, 0);
    iDescWidth  = workArea.right - workArea.left;
    iDescHeight = workArea.bottom - workArea.top;

    // compensate for window border
    if(info.cxWindowBorders && info.cyWindowBorders)
    {
        iWidth  = iWidth + (info.rcClient.left - info.rcWindow.left) + (info.rcWindow.right - info.rcClient.right);
        iHeight = iHeight + (info.rcClient.top - info.rcWindow.top) + (info.rcWindow.bottom - info.rcClient.bottom);
    }

    // shift window if too large
    if(iWidth > iDescWidth)
    {
        info.rcWindow.left = workArea.left;
        iWidth = iDescWidth;
    }
    else if((info.rcWindow.left + iWidth) > iDescWidth)
        info.rcWindow.left -= ((iWidth + info.rcWindow.left) - iDescWidth);
    else if(info.rcWindow.left < workArea.left)
        info.rcWindow.left = workArea.left;

    if(iHeight > iDescHeight)
    {
        info.rcWindow.top = workArea.top;
        iHeight = iDescHeight;
    }
    else if((info.rcWindow.top + iHeight) > iDescHeight)
        info.rcWindow.top -= ((iHeight + info.rcWindow.top) - iDescHeight);
    else if(info.rcWindow.top < workArea.top)
        info.rcWindow.top = workArea.top;

    m_wndRect.x      = info.rcWindow.left;
    m_wndRect.y      = info.rcWindow.top;
    m_wndRect.width  = iWidth;
    m_wndRect.height = iHeight;
    MoveWindow(m_hWnd, m_wndRect.x, m_wndRect.y, m_wndRect.width, m_wndRect.height, TRUE);
}

void WindowWin::WindowSetFullscreen(bool bFullscreen)
{
    if(!m_bWndCreated || m_iStyle == WSTYLE_FIXED_SIZE)
        return;

    if(bFullscreen == m_bFullscreen)
        return;

    if(!bFullscreen)
    {
        ShowWindow(m_hWnd, SW_RESTORE);
        SetWindowLongPtr(m_hWnd, GWL_STYLE, WS_OVERLAPPEDWINDOW | WS_VISIBLE);
        SetWindowPos(m_hWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_FRAMECHANGED);
        m_bFullscreen = false;
    }
    else
    {
        SetWindowLongPtr(m_hWnd, GWL_STYLE, WS_POPUP | WS_VISIBLE);
        SetWindowPos(m_hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_FRAMECHANGED);
        ShowWindow(m_hWnd, SW_SHOWMAXIMIZED);
        m_bFullscreen = true;
    }
}

void WindowWin::WindowShow()
{
    if(!m_bWndCreated)
        return;

    ShowWindow(m_hWnd, SW_SHOWNORMAL);
}

void WindowWin::WindowHide()
{
    if(!m_bWndCreated)
        return;

    ShowWindow(m_hWnd, SW_HIDE);
}

void WindowWin::WindowGetSize(unsigned int &iWidth, unsigned int &iHeight)
{
    if(!m_bWndCreated)
        return;

    ::RECT rect;
    GetClientRect(m_hWnd, &rect);
    iWidth   = rect.right;
    iHeight  = rect.bottom;
}

void WindowWin::GetWindowContext(RendererContext *pContext)
{
    if(!m_bWndCreated)
        return;

    if(!pContext)
        return;

    pContext->m_hWnd = m_hWnd;
}

void WindowWin::WaitMessages()
{
    if(!m_bWndCreated)
        return;

    if(GetMessage(&m_message, m_hWnd, 0, 0))
    {
        TranslateMessage(&m_message);
        DispatchMessage(&m_message);
    }
}

void WindowWin::CheckMessages()
{
    if(!m_bWndCreated)
        return;

    if(PeekMessage(&m_message, m_hWnd, 0, 0, PM_REMOVE))
    {
        TranslateMessage(&m_message);
        DispatchMessage(&m_message);
    }
}

void WindowWin::Invalidate()
{
    if(!m_bWndCreated)
        return;

    InvalidateRect(m_hWnd, 0, 0);
}

void WindowWin::AddKeyboardCallback(KeyboardCallback keyCall, void* pParams)
{
    m_feedback.keyCall = keyCall;
    m_feedback.keyCallParams = pParams;
}
#endif

#endif
