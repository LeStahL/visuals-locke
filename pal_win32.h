/* Hardcyber - PC-64k-Intro by Team210 at Deadline 2k19
 * Copyright (C) 2019 DaDummy <c.anselm@paindevs.com>
 * Copyright (C) 2019 Alexander Kraus <nr4@z10.info>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef PAL_WIN32_H
#define PAL_WIN32_H

#include "common.h"


void *malloc(size_t size)
{
	return GlobalAlloc(GMEM_ZEROINIT, size);
}

HWAVEOUT hWaveOut;
WAVEHDR header = { 0, 0, 0, 0, 0, 0, 0, 0 };

HDC hdc;
HGLRC glrc;

#ifdef RECORD
HWND hRecordFilenameEdit, hCaptureWindow, hCaptureDriverComboBox ;
#endif 

double get_sound_playback_time();
void set_sound_playback_time(double time);
void set_sound_playback_range(double time_begin, double time_end);

int flip_buffers()
{
	SwapBuffers(hdc);

	MSG msg = { 0 };
	while ( PeekMessageA( &msg, NULL, 0, 0, PM_REMOVE ) )
	{
		if ( msg.message == WM_QUIT ) {
			return FALSE;
		}
		TranslateMessage( &msg );
		DispatchMessageA( &msg );
	}

	return TRUE;
}

void transfer_text()
{
    glBindTexture(GL_TEXTURE_2D, input_texture_handle);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, input_texture_size, input_texture_size, 0, GL_RGBA, GL_UNSIGNED_BYTE, input);
    glBindTexture(GL_TEXTURE_2D, 0);
}

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch(uMsg)
	{
		case WM_KEYDOWN:
			switch(wParam)
			{
				case VK_ESCAPE:
#ifdef RECORD
                    if(recording) 
                    {
                        capFileSaveAs(hCaptureWindow, record_filename);
                    }
#endif
					ExitProcess(0);
					break;
				case VK_SPACE:
					break;
                case VK_BACK:
                    input[ninputs-1] = '\0';
                    input[ninputs] = '\0';
//                     input[ninputs-2] = '\0';
                    ninputs = max(ninputs-2,0);
                    printf("%d >> %s\n", ninputs, input);
                    transfer_text();
                    break;
                case VK_RETURN:
                    show_window = 1.-show_window;
                    break;
                case VK_CONTROL:
                    printf("control.\n");
                    scale_override = 1.;
                    break;
			}
			break;
		case WM_RBUTTONDOWN:
#ifdef RECORD
            if(recording) capFileSaveAs(hCaptureWindow, record_filename);
#endif
			ExitProcess(0);
			break;
        case WM_CHAR:
            char c = tolower(wParam);
            if(c == 'ö') c = 'o';
            if(c == 'ä') c = 'a';
            if(c == 'ü') c = 'u';
            if(c == 'ß') c = '3';
            input[ninputs] = c;
            input[ninputs+1] = '\0';
            ninputs = min(ninputs + 1, input_texture_nentries-2);
            printf("%s\n", input);
            transfer_text();
            break;
        case WM_KEYUP:
            switch(wParam)
			{
                case VK_CONTROL:
                    scale_override = 0.;
                    break;
            }
            break;
            
		default:
			break;

	}
	return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

LRESULT CALLBACK DialogProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	switch(uMsg)
	{
		case WM_COMMAND:
			UINT id =  LOWORD(wParam);
			HWND hSender = (HWND)lParam;

			switch(id)
			{
				case 5:
				{
					int index = SendMessage(hSender, CB_GETCURSEL, 0, 0);
					w = widths[index];
                    h = heights[index];
				}
                break;
				case 6:
					muted = !muted;
					if(muted)
						SendMessage(hSender, BM_SETCHECK, BST_CHECKED, 0);
					else
						SendMessage(hSender, BM_SETCHECK, BST_UNCHECKED, 0);
                break;
				case 7:
#ifdef RECORD
                    GetWindowText(hRecordFilenameEdit, record_filename, 1024);
#endif
					DestroyWindow(hwnd);
					PostQuitMessage(0);
                break;
				case 8: // Full screen Antialiasing
				{
					int index = SendMessage(hSender, CB_GETCURSEL, 0, 0);
					fsaa = (index + 1)*(index + 1);
				}
                break;
				case 9: // Texture buffer size
				{
					int index = SendMessage(hSender, CB_GETCURSEL, 0, 0);
					texs = 128;
					for(int i=0; i<index; ++i)
						texs *= 2;
					block_size = texs*texs;
				}
                break;
				case 10:
				{
                    start_at_scene = SendMessage(hSender, CB_GETCURSEL, 0, 0);
				}
				break;
#ifdef RECORDING
                case 11:
                {
                    recording = !recording;
					if(recording)
                    {
						SendMessage(hSender, BM_SETCHECK, BST_CHECKED, 0);
                        EnableWindow(hRecordFilenameEdit, TRUE);
                        EnableWindow(hCaptureDriverComboBox, TRUE);
                    }
                    else
                    {
						SendMessage(hSender, BM_SETCHECK, BST_UNCHECKED, 0);
                        EnableWindow(hRecordFilenameEdit, FALSE);
                        EnableWindow(hCaptureDriverComboBox, FALSE);
                    }
                }
				break;
#endif
			}
			break;

		case WM_CLOSE:
			ExitProcess(0);
			break;
	}
	return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

int WINAPI demo(HINSTANCE hInstance, HINSTANCE hPrevInstance, PWSTR pCmdLine, int nCmdShow)
{
#ifdef DEBUG
	AllocConsole();
	freopen("CONIN$", "r", stdin);
	freopen("CONOUT$", "w", stdout);
	freopen("CONOUT$", "w", stderr);
#endif

	// Display settings selector
	WNDCLASS wca = { 0 };
	wca.lpfnWndProc   = DialogProc;
	wca.hInstance     = hInstance;
	wca.lpszClassName = L"Settings";
	RegisterClass(&wca);
	HWND lwnd = CreateWindowEx(
		0,                              // Optional window styles.
		L"Settings",                     // Window class
		demoname,    // Window text
		WS_OVERLAPPEDWINDOW,            // Window style

		// Size and position
		200, 200, 300, 360,

		NULL,       // Parent window
		NULL,       // Menu
		hInstance,  // Instance handle
		NULL        // Additional application data
		);

	// Add "Resolution: " text
	HWND hResolutionText = CreateWindow(WC_STATIC, "Resolution: ", WS_VISIBLE | WS_CHILD | SS_LEFT, 10,15,100,100, lwnd, NULL, hInstance, NULL);

	// Add resolution Combo box
	HWND hResolutionComboBox = CreateWindow(WC_COMBOBOX, TEXT(""),
	 CBS_DROPDOWN | CBS_HASSTRINGS | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE,
	 100, 10, 175, 400, lwnd, (HMENU)5, hInstance,
	 NULL);

	// Add items to resolution combo box and select full HD
    for(int i=0; i<nresolutions; ++i)
        SendMessage(hResolutionComboBox, (UINT) CB_ADDSTRING, (WPARAM) 0, (LPARAM) resolution_names[i]);
	SendMessage(hResolutionComboBox, CB_SETCURSEL, 2, 0);
    w = widths[2];
    h = heights[2];

	// Add mute checkbox
	HWND hMuteCheckbox = CreateWindow(WC_BUTTON, TEXT("Mute"),
					 WS_VISIBLE | WS_CHILD | BS_CHECKBOX,
					 10, 40, 100, 20,
					 lwnd, (HMENU) 6, hInstance, NULL);

	// Add "Antialiasing: " text
	HWND hAntialiasingText = CreateWindow(WC_STATIC, "FSAA: ", WS_VISIBLE | WS_CHILD | SS_LEFT, 10,65,100,100, lwnd, NULL, hInstance, NULL);

	// Add Fullscreen Antialiasing combo box
	HWND hFSAAComboBox= CreateWindow(WC_COMBOBOX, TEXT(""),
	 CBS_DROPDOWN | CBS_HASSTRINGS | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE,
	 100, 60, 175, 280, lwnd, (HMENU)8, hInstance,
	 NULL);
    
    // Populate with entries
    for(int i=0; i<nfsaa; ++i)
        SendMessage(hFSAAComboBox, (UINT) CB_ADDSTRING, (WPARAM) 0, (LPARAM) fsaa_names[i]);
	SendMessage(hFSAAComboBox, CB_SETCURSEL, nfsaa-1, 0);
    fsaa = nfsaa*nfsaa;

	// Add "SFX Buffer: " text
	HWND hTXAAText = CreateWindow(WC_STATIC, "SFX Buffer: ", WS_VISIBLE | WS_CHILD | SS_LEFT, 10,95,100,100, lwnd, NULL, hInstance, NULL);

	// Add SFX buffer size combo box
	HWND hTXAAComboBox = CreateWindow(WC_COMBOBOX, TEXT(""),
	 CBS_DROPDOWN | CBS_HASSTRINGS | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE,
	 100, 90, 175, 280, lwnd, (HMENU)9, hInstance,
	 NULL);

	// Populate with entries
    for(int i=0; i<4; ++i)
        SendMessage(hTXAAComboBox, (UINT) CB_ADDSTRING, (WPARAM) 0, (LPARAM) buffersize_names[i]);
	SendMessage(hTXAAComboBox, CB_SETCURSEL, 2, 0);

	// Add "Antialiasing: " text
	HWND hSceneText = CreateWindow(WC_STATIC, "Scene: ", WS_VISIBLE | WS_CHILD | SS_LEFT, 10,125,100,100, lwnd, NULL, hInstance, NULL);

	// Add scene selector
	HWND hSceneComboBox = CreateWindow(WC_COMBOBOX, TEXT(""),
	 CBS_DROPDOWN | CBS_HASSTRINGS | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE,
	 100, 120, 175, 680, lwnd, (HMENU)10, hInstance,
	 NULL);

    // Populate with entries
    for(int i=0; i<nscenes; ++i)
        SendMessage(hSceneComboBox, (UINT) CB_ADDSTRING, (WPARAM) 0, (LPARAM) scene_names[i]);
	SendMessage(hSceneComboBox, CB_SETCURSEL, 0, 0);

	// Add start button
	HWND hwndButton = CreateWindow(WC_BUTTON,"Offend!",WS_TABSTOP | WS_VISIBLE | WS_CHILD | BS_DEFPUSHBUTTON,185,225,90,90,lwnd,(HMENU)7,hInstance,NULL);

#ifdef RECORD
    // Add record checkbox
	HWND hRecordCheckbox = CreateWindow(WC_BUTTON, TEXT("Record"),
					 WS_VISIBLE | WS_CHILD | BS_CHECKBOX,
					 10, 150, 89, 20,
					 lwnd, (HMENU) 11, hInstance, NULL);
    
    // Add record filename text field
    hRecordFilenameEdit = CreateWindow(WC_EDIT, TEXT("lightcyber.avi"), WS_VISIBLE | WS_CHILD | WS_BORDER ,100,150 ,175,25,lwnd, (HMENU) 12,NULL,NULL );
    EnableWindow(hRecordFilenameEdit, FALSE);
    
    // Add capture driver selector
	hCaptureDriverComboBox = CreateWindow(WC_COMBOBOX, TEXT(""),
        CBS_DROPDOWN | CBS_HASSTRINGS | WS_CHILD | WS_OVERLAPPED | WS_VISIBLE,
        100, 180, 175, 680, lwnd, (HMENU)12, hInstance,
        NULL);
    char capture_device_name[80];
    char capture_device_version[80];

    for(int wIndex = 0; wIndex < 10; wIndex++) 
    {
        if (capGetDriverDescription(
                wIndex, 
                capture_device_name, 
                sizeof (capture_device_name), 
                capture_device_version, 
                sizeof (capture_device_version)
            )) 
        {
            SendMessage(hCaptureDriverComboBox, (UINT) CB_ADDSTRING, (WPARAM) 0, (LPARAM) capture_device_name);
        }
    } 
    SendMessage(hCaptureDriverComboBox, CB_SETCURSEL, 0, 0);
    EnableWindow(hCaptureDriverComboBox, FALSE);
#endif
    
	// Show the selector
	ShowWindow(lwnd, TRUE);
	UpdateWindow(lwnd);

	MSG msg = { 0 };
	while(GetMessage(&msg, NULL, 0, 0) > 0)
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

#if RECORDING
	if(recording)
    {
        // FIXME: add actually selected driver
        SendMessage (hCaptureWindow, WM_CAP_DRIVER_CONNECT, 0, 0L); 
    }
#endif

#ifdef DEBUG
	printf("Rendering Demo with:\nSound ");
    if(muted)printf("muted");
    else printf("playing");
	printf("\nResolution: %d * %d\n", w, h);
	printf("FSAA: %d*\n", fsaa);
    if(recording)printf("recording to %s\n", record_filename);
#endif

	// Display demo window
	CHAR WindowClass[]  = "Team210 Demo Window";

	WNDCLASSEX wc = { 0 };
	wc.cbSize = sizeof(wc);
	wc.style = CS_OWNDC | CS_VREDRAW | CS_HREDRAW;
	wc.lpfnWndProc = &WindowProc;
	wc.cbClsExtra = 0;
	wc.cbWndExtra = 0;
	wc.hInstance = hInstance;
	wc.hIcon = LoadIcon(NULL, IDI_WINLOGO);
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground = NULL;
	wc.lpszMenuName = NULL;
	wc.lpszClassName = WindowClass;
	wc.hIconSm = NULL;

	RegisterClassEx(&wc);

	// Create the window.
    HWND hwnd = CreateWindowEx(
            0,                                                          // Optional window styles.
            WindowClass,                                                // Window class
            ":: Team210 :: GO - MAKE A DEMO ::",                                 // Window text
            WS_POPUP | WS_VISIBLE,                                      // Window style
            0,
            0,
            w,
            h,                     // Size and position

            NULL,                                                       // Parent window
            NULL,                                                       // Menu
            hInstance,                                                  // Instance handle
            0                                                           // Additional application data
        );
#ifdef RECORD
    if(recording)
    {
        hCaptureWindow = capCreateCaptureWindow(
            WindowClass,
            WS_CHILD | WS_VISIBLE,
            0,
            0,
            w,
            h,
            hwnd,
            0                                                
        );
        capCaptureSequence(hCaptureWindow); 
    }
#endif
    
    DEVMODE dm = { 0 };
    dm.dmSize = sizeof(dm);
    dm.dmPelsWidth = w;
    dm.dmPelsHeight = h;
    dm.dmFields = DM_PELSWIDTH | DM_PELSHEIGHT;
    
    ChangeDisplaySettings(&dm, CDS_FULLSCREEN);
    
	// Show it
	ShowWindow(hwnd, TRUE);
	UpdateWindow(hwnd);

	// Create OpenGL context
	PIXELFORMATDESCRIPTOR pfd =
	{
		sizeof(PIXELFORMATDESCRIPTOR),
		1,
		PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER,    //Flags
		PFD_TYPE_RGBA,        // The kind of framebuffer. RGBA or palette.
		32,                   // Colordepth of the framebuffer.
		0, 0, 0, 0, 0, 0,
		0,
		0,
		0,
		0, 0, 0, 0,
		24,                   // Number of bits for the depthbuffer
		8,                    // Number of bits for the stencilbuffer
		0,                    // Number of Aux buffers in the framebuffer.
		PFD_MAIN_PLANE,
		0,
		0, 0, 0
	};

	hdc = GetDC(hwnd);

	int  pf = ChoosePixelFormat(hdc, &pfd);
	SetPixelFormat(hdc, pf, &pfd);

	glrc = wglCreateContext(hdc);
	wglMakeCurrent(hdc, glrc);

    rInitializeRenderer();

    ShowCursor(FALSE);
    
	load_demo();

    jump_to_scene(start_at_scene);
    
    QueryPerformanceCounter((LARGE_INTEGER*)&current_time);
    QueryPerformanceFrequency((LARGE_INTEGER*)&cps);
    offset = (double)current_time/(double)cps;

    //FFTW3 Setup
    in = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * NFFT);
    out = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * NFFT);
    p = fftw_plan_dft_1d(NFFT, in, out, FFTW_FORWARD, FFTW_ESTIMATE);
	
    // Init sound capture
    WAVEFORMATEX wfx;
    wfx.wFormatTag = WAVE_FORMAT_PCM;
    wfx.nChannels = 1;                    
    wfx.nSamplesPerSec = 44100.;
    wfx.wBitsPerSample = 16;
    wfx.nBlockAlign = wfx.wBitsPerSample * wfx.nChannels / 8;
    wfx.nAvgBytesPerSec = wfx.nBlockAlign * wfx.nSamplesPerSec;
    
    // Prepare mic
    int result = waveInOpen(&wi,            
                WAVE_MAPPER,    
                &wfx,           
                NULL,NULL,      
                CALLBACK_NULL | WAVE_FORMAT_DIRECT  
              );
    printf("WaveInOpen: %d\n", result);
    
    int bsize = buffer_size*wfx.wBitsPerSample*wfx.nChannels/8;
    char * buffers;
    if(double_buffered == 1)
        buffers = (char*)malloc(2*bsize);
    else
        buffers = (char*)malloc(bsize);
    
    for(int i = 0; i < double_buffered+1; ++i)
    {
        printf("Buffer i:\n");
        headers[i].lpData =         buffers+i*bsize;             
        headers[i].dwBufferLength = bsize;
        result = waveInPrepareHeader(wi, &headers[i], sizeof(headers[i]));
        printf("WaveInPrepareHeader: %d\n", result);
        result = waveInAddBuffer(wi, &headers[i], sizeof(headers[i]));
        printf("WaveInAddBuffer: %d\n", result);
    }
    
    result = waveInStart(wi);
    printf("WaveInStart: %d\n", result);
    
    // Main loop
    while(flip_buffers())
	{
        t_now = get_sound_playback_time();

		draw();
	}

	return 0;
}

void jump_to_scene(unsigned int scene_index)
{
    if (scene_index < nscenes)
    {
        set_sound_playback_range(start_times[scene_index], duration);
    }
    else
    {
        printf("Midi scene override failed - index out of bounds: %d", scene_index);
    }
}

// void initialize_sound()
// {
// 	hWaveOut = 0;
// 	int n_bits_per_sample = 16;
// 	WAVEFORMATEX wfx = { WAVE_FORMAT_PCM, channels, sample_rate, sample_rate*channels*n_bits_per_sample / 8, channels*n_bits_per_sample / 8, n_bits_per_sample, 0 };
// 	waveOutOpen(&hWaveOut, WAVE_MAPPER, &wfx, 0, 0, CALLBACK_NULL);
// 
//     t_start = t_now = 0.;
//     t_end = duration;
// }

double get_sound_playback_time()
{
    QueryPerformanceCounter((LARGE_INTEGER*)&current_time);
//     QueryPerformanceFrequency((LARGE_INTEGER*)&cps);
    return (double)current_time/(double)cps-offset;
    
//     static MMTIME MMTime = { TIME_SAMPLES, 0};
//     waveOutGetPosition(hWaveOut, &MMTime, sizeof(MMTIME));
//     return t_start + ((double)MMTime.u.sample) / 44100.0;
}

void set_sound_playback_time(double time_begin)
{
    set_sound_playback_range(time_begin, t_end);
}

void set_sound_playback_range(double time_begin, double time_end)
{
//     waveOutReset(hWaveOut);

    t_start = time_begin;
    t_end = time_end;
//     int delta = clamp((int)(t_start * (double)sample_rate), 0, music1_size-1);
//     header.lpData = smusic1 + delta;
//     header.dwBufferLength = 4 * (music1_size-delta);
//     waveOutPrepareHeader(hWaveOut, &header, sizeof(WAVEHDR));
//     waveOutWrite(hWaveOut, &header, sizeof(WAVEHDR));
}

#endif
