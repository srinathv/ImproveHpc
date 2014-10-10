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

#ifndef __BASE_WINDOW_H__
#define __BASE_WINDOW_H__

#include "base_image.h"
#include "vm_thread.h"

#ifdef ENABLE_RENDERING
#ifdef _WIN32
#include <windows.h>

struct RendererContext
{
    RendererContext()
    {
        m_hWnd = NULL;
    }

    HWND m_hWnd;
};
#endif

#ifdef UNIX
#include <X11/Xlib.h>
#include <X11/Xutil.h>

struct RendererContext
{
    RendererContext()
    {
        m_pVisualInfo = NULL;
        m_pDisplay    = NULL;
        m_iScreen     = 0;
    }

    XVisualInfo* m_pVisualInfo;
    Display*     m_pDisplay;
    Window       m_window;
    unsigned int m_iScreen;
};
#endif
#else
#define KK_F      0
#define KK_ESCAPE 0
#define KK_SPACE  0

struct RendererContext
{
    RendererContext()
    {
    }
};
#endif

enum KeyFlags
{
    KF_MESSAGE_UP   = 0x00000001,
    KF_STATE_CHANGE = 0x00000002,
    KF_EXT_KEY      = 0x00000004
};

typedef void (*KeyboardCallback)(int iKeyCode, int iCount, unsigned int iFlags, void *pParams);

struct WndFeedback
{
    WndFeedback()
    {
        iLastKey   = -1;
        iLastState = -1;

        keyCall         = NULL;
        keyCallParams   = NULL;

        bDestroyMessage = false;
        iRepaint        = 0;
    }

    int iLastKey;
    int iLastState;

    KeyboardCallback  keyCall;
    void             *keyCallParams;

    bool bDestroyMessage;
    volatile int iRepaint;
};

enum WindowStyle
{
    WSTYLE_NORMAL,
    WSTYLE_FIXED_SIZE,
    WSTYLE_BORDERLESS
};

struct WndRect
{
    int x;
    int y;
    unsigned int width;
    unsigned int height;
};

// base dummy class
class WindowBase
{
public:
    WindowBase()
    {
        m_wndRect.x     = m_wndRect.y      = 0;
        m_wndRect.width = m_wndRect.height = 0;
        m_iStyle        = WSTYLE_NORMAL;
        m_bFullscreen   = false;
        m_bWndCreated   = false;
    }

    virtual ~WindowBase()
    {
        WindowClose();
    }

    virtual bool WindowCreate(const char*, unsigned int = WSTYLE_NORMAL) { return false; };
    virtual void WindowSetRect(WndRect) { return; };
    virtual void WindowResize(unsigned int, unsigned int) { return; };
    virtual void WindowSetFullscreen(bool) { return; };
    virtual void WindowShow() { return; };
    virtual void WindowHide() { return; };
    virtual void WindowClose() { return; };
    virtual void WindowGetSize(unsigned int &, unsigned int &) { return; };
    virtual void GetWindowContext(RendererContext*) { return; };
    virtual void Invalidate() { return; };
    virtual void WaitMessages() { return; };
    virtual void CheckMessages() { return; };

    virtual void AddKeyboardCallback(KeyboardCallback, void*) { return; };

    bool IsWindowExist() { return m_bWndCreated; };

    void WindowSwitchFullscreen()
    {
        WindowSetFullscreen(!m_bFullscreen);
    }

public:
    WndFeedback   m_feedback;

protected:
    WndRect       m_wndRect;
    unsigned int  m_iStyle;
    bool          m_bFullscreen;
    bool          m_bWndCreated;
};

#ifdef ENABLE_RENDERING

#ifdef _WIN32
#define KK_0 0x30
#define KK_1 0x31
#define KK_2 0x32
#define KK_3 0x33
#define KK_4 0x34
#define KK_5 0x35
#define KK_6 0x36
#define KK_7 0x37
#define KK_8 0x38
#define KK_9 0x39

#define KK_A 0x41
#define KK_B 0x42
#define KK_C 0x43
#define KK_D 0x44
#define KK_E 0x45
#define KK_F 0x46
#define KK_G 0x47
#define KK_H 0x48
#define KK_I 0x49
#define KK_J 0x4A
#define KK_K 0x4B
#define KK_L 0x4C
#define KK_M 0x4D
#define KK_N 0x4E
#define KK_O 0x4F
#define KK_P 0x50
#define KK_Q 0x51
#define KK_R 0x52
#define KK_S 0x53
#define KK_T 0x54
#define KK_U 0x55
#define KK_V 0x56
#define KK_W 0x57
#define KK_X 0x58
#define KK_Y 0x59
#define KK_Z 0x5A

#define KK_ESCAPE VK_ESCAPE
#define KK_SPACE  VK_SPACE
#define KK_ENTER  VK_RETURN
#define KK_LEFT   VK_LEFT
#define KK_RIGHT  VK_RIGHT

class WindowWin : public WindowBase
{
public:
    WindowWin();
    virtual ~WindowWin();

    virtual bool WindowCreate(const char* cAppName, unsigned int iStyle = WSTYLE_NORMAL);
    virtual void WindowSetRect(WndRect wndRect);
    virtual void WindowResize(unsigned int iWidth, unsigned int iHeight);
    virtual void WindowSetFullscreen(bool bFullscreen);
    virtual void WindowShow();
    virtual void WindowHide();
    virtual void WindowClose();
    virtual void WindowGetSize(unsigned int &iWidth, unsigned int &iHeight);
    virtual void GetWindowContext(RendererContext *pContext);
    virtual void Invalidate();
    virtual void WaitMessages();
    virtual void CheckMessages();

    virtual void AddKeyboardCallback(KeyboardCallback keyCall, void* pParams);

protected:
    HWND   m_hWnd;
    MSG    m_message;
    HMENU *m_pMenu;
    LONG   m_WndStyle;
    LONG   m_WndStyleEx;
};
#endif


#ifdef UNIX
#include <X11/keysym.h>

#ifndef XK_0
#define KK_0 0x30
#define KK_1 0x31
#define KK_2 0x32
#define KK_3 0x33
#define KK_4 0x34
#define KK_5 0x35
#define KK_6 0x36
#define KK_7 0x37
#define KK_8 0x38
#define KK_9 0x39
else
#define KK_0 XK_0
#define KK_1 XK_1
#define KK_2 XK_2
#define KK_3 XK_3
#define KK_4 XK_4
#define KK_5 XK_5
#define KK_6 XK_6
#define KK_7 XK_7
#define KK_8 XK_8
#define KK_9 XK_9
#endif

#ifndef XK_a
#define KK_A 0x61
#define KK_B 0x62
#define KK_C 0x63
#define KK_D 0x64
#define KK_E 0x65
#define KK_F 0x66
#define KK_G 0x67
#define KK_H 0x68
#define KK_I 0x69
#define KK_J 0x6A
#define KK_K 0x6B
#define KK_L 0x6C
#define KK_M 0x6D
#define KK_N 0x6E
#define KK_O 0x6F
#define KK_P 0x70
#define KK_Q 0x71
#define KK_R 0x72
#define KK_S 0x73
#define KK_T 0x74
#define KK_U 0x75
#define KK_V 0x76
#define KK_W 0x77
#define KK_X 0x78
#define KK_Y 0x79
#define KK_Z 0x7A
#else
#define KK_A XK_a
#define KK_B XK_b
#define KK_C XK_c
#define KK_D XK_d
#define KK_E XK_e
#define KK_F XK_f
#define KK_G XK_g
#define KK_H XK_h
#define KK_I XK_i
#define KK_J XK_j
#define KK_K XK_k
#define KK_L XK_l
#define KK_M XK_m
#define KK_N XK_n
#define KK_O XK_o
#define KK_P XK_p
#define KK_Q XK_q
#define KK_R XK_r
#define KK_S XK_s
#define KK_T XK_t
#define KK_U XK_u
#define KK_V XK_v
#define KK_W XK_w
#define KK_X XK_x
#define KK_Y XK_y
#define KK_Z XK_z
#endif

#define KK_ESCAPE XK_Escape
#define KK_SPACE  XK_space

class WindowX : public WindowBase
{
public:
    WindowX();
    virtual ~WindowX();

    virtual bool WindowCreate(const char* cAppName, unsigned int iStyle = WSTYLE_NORMAL);
    virtual void WindowSetRect(WndRect wndRect);
    virtual void WindowResize(unsigned int iWidth, unsigned int iHeight);
    virtual void WindowSetFullscreen(bool bFullscreen);
    virtual void WindowShow();
    virtual void WindowHide();
    virtual void WindowClose();
    virtual void WindowGetSize(unsigned int &iWidth, unsigned int &iHeight);
    virtual void GetWindowContext(RendererContext *pContext);
    virtual void Invalidate();
    virtual void WaitMessages();
    virtual void CheckMessages();
    void WindowProc(XEvent &event);

    virtual void AddKeyboardCallback(KeyboardCallback keyCall, void* pParams);

protected:
    Display*     m_display;
    Window       m_window;
    XVisualInfo* m_pvisinfo;
    unsigned int m_iScreen;
    Atom         m_wmDelete;
    Time         m_iPrevPressTime;
};
#endif

#endif

#endif
