/* Hardcyber - PC-64k-Intro by Team210 at Deadline 2k19
 * Copyright (C) 2019 Alexander Kraus <nr4@z10.info>
 * Copyright (C) 2019 DaDummy <c.anselm@paindevs.com>
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

#define DEBUG // Shader debug i/o
#define DEBUG_SHADER // Shader compile and link errors
#define MIDI // APC40 mkII controls
// #define RECORD // Compile in recording capabilities

#define DEMO

const char *demoname = "DJ LOCKE B.SC. FEAT. M.SC. NR4/Team210";
unsigned int muted = 0.;

int _fltused = 0;

#include "common.h"

// Standard library and CRT rewrite for saving executable size
void *memset(void *ptr, int value, size_t num)
{
    for(int i=num-1; i>=0; i--)
        ((unsigned char *)ptr)[i] = value;
    return ptr;
}

size_t strlen(const char *str)
{
    int len = 0;
    while(str[len] != '\0') ++len;
    return len;
}

#ifdef MIDI

int btns = 0;
void select_button(int _index)
{
    int nfiles = ARRAYSIZE(shader_programs);
    if(_index < 40)
    {
        for(int i=0; i<nfiles; ++i)
        {
            DWORD out_msg = 0x9 << 4 | i << 8 | 72 << 16;
            midiOutShortMsg(hMidiOut, out_msg);
        }
        for(int i=nfiles; i<40; ++i)
        {
           DWORD out_msg = 0x8 << 4 | i << 8 | 0 << 16;
            midiOutShortMsg(hMidiOut, out_msg);
        }
        
        override_index = _index;
        
        DWORD out_msg = 0x9 << 4 | _index << 8 | 13 << 16;
        midiOutShortMsg(hMidiOut, out_msg);
    }
    else 
    {
//         dt_interval = (_index - 0x52) % 5;
        
//         for(int i=dt_interval; i<5; ++i)
//         {
//             DWORD out_msg = 0x9 << 4 | (0x52 + i) << 8 | 67 << 16;
//             midiOutShortMsg(hMidiOut, out_msg);
//         }
//         for(int i=0; i<=dt_interval; ++i)
//         {
//             DWORD out_msg = 0x9 << 4 | (0x52 + i) << 8 | 95 << 16;
//             midiOutShortMsg(hMidiOut, out_msg);
//         }
    }
}

#define NOTE_OFF 0x8
#define NOTE_ON 0x9
#define CONTROL_CHANGE 0xB

#define TIME_DIAL 0x14
#define TIME_FINE_DIAL 0x15
#define TIME_VERYFINE_DIAL 0x16

void CALLBACK MidiInProc_apc40mk2(HMIDIIN hMidiIn, UINT wMsg, DWORD dwInstance, DWORD dwParam1, DWORD dwParam2)
{
    if(wMsg == MIM_DATA)
    {
        BYTE b1 = (dwParam1 >> 24) & 0xFF,
            b2 = (dwParam1 >> 16) & 0xFF,
            b3 = (dwParam1 >> 8) & 0xFF,
            b4 = dwParam1 & 0xFF;
        BYTE b3lo = b3 & 0xF,
            b3hi = (b3 >> 4) & 0xF,
            b4lo = b4 & 0xF,
            b4hi = (b4 >> 4) & 0xF;
        
        BYTE channel = b4lo,
            button = b3;
            
        if(b4hi == NOTE_ON)
        {   
        }
        else if(b4hi == NOTE_OFF)
        {
            select_button(button);
            
            // Logo 210
            if(button == 0x59)
            {
                char data[40] = 
                {
                    1,  1,  0,  1,  0,  1,  1,  0,
                    12, 12, 1,  1,  1,  12, 12, 1,
                    0,  0,  1,  1,  1,  0,  0,  1,
                    0,  0,  1,  1,  1,  0,  0,  1,
                    1,  1,  12, 1,  12, 1,  1,  12
                };

                for(int i=0; i<40; ++i)
                {
                    
                    DWORD out_msg;
                    if(data[i] == 0) 
                    {
                        out_msg = 0x8 << 4 | i << 8 | 0 << 16;
                    }
                    else
                    {
                        out_msg = 0x9 << 4 | i << 8 | 1+(data[i]+btns) %125 << 16;
                    }
                    midiOutShortMsg(hMidiOut, out_msg);
                }
                btns = 1+(btns+1)%125;
            }
            // Kewlers Logo
            else if(button == 0x57)
            {
                char data[40] = 
                {
                    3,3,3,3,1,1,1,1,
                    3,3,3,3,1,1,1,1,
                    3,3,3,0,0,1,1,1,
                    0,3,0,0,0,0,1,0,
                    0,7,0,0,0,0,9,0
                };

                for(int i=0; i<40; ++i)
                {
                    
                    DWORD out_msg;
                    if(data[i] == 0) 
                    {
                        out_msg = 0x8 << 4 | i << 8 | 0 << 16;
                    }
                    else
                    {
                        out_msg = 0x9 << 4 | i << 8 | 1+(data[i]+btns) %125 << 16;
                    }
                    midiOutShortMsg(hMidiOut, out_msg);
                }
                btns = 1+(btns+1)%125;
            }
        }
        else if(b4hi == CONTROL_CHANGE)// Channel select
        {
            if(button == TIME_DIAL)
            {
//                 waveOutReset(hWaveOut);
                time_dial = (double)b2/(double)0x7F;
                
//                 int delta = (.9*time_dial+.09*time_fine_dial+.01*time_very_fine_dial) * duration * (double)sample_rate;
//                 header.lpData = min(max(smusic1, smusic1+delta), smusic1+music1_size);
//                 header.dwBufferLength = 4 * (music1_size-delta);
//                 waveOutPrepareHeader(hWaveOut, &header, sizeof(WAVEHDR));
//                 waveOutWrite(hWaveOut, &header, sizeof(WAVEHDR));
//                 waveOutPause(hWaveOut);
                paused = 1;
            }
            else if(button == TIME_FINE_DIAL)
            {
//                 waveOutReset(hWaveOut);
                time_fine_dial = (double)b2/(double)0x7F;
                
//                 int delta = (.9*time_dial+.09*time_fine_dial+.01*time_very_fine_dial) * duration * (double)sample_rate;
//                 header.lpData = min(max(smusic1, smusic1+delta), smusic1+music1_size);
//                 header.dwBufferLength = 4 * (music1_size-delta);
//                 waveOutPrepareHeader(hWaveOut, &header, sizeof(WAVEHDR));
//                 waveOutWrite(hWaveOut, &header, sizeof(WAVEHDR));
//                 waveOutPause(hWaveOut);
                paused = 1;
            }
            else if(button == TIME_VERYFINE_DIAL)
            {
//                 waveOutReset(hWaveOut);
                time_very_fine_dial = (double)b2/(double)0x7F;
                
//                 int delta = (.9*time_dial+.09*time_fine_dial+.01*time_very_fine_dial) * duration * (double)sample_rate;
//                 header.lpData = min(max(smusic1, smusic1+delta), smusic1+music1_size);
//                 header.dwBufferLength = 4 * (music1_size-delta);
//                 waveOutPrepareHeader(hWaveOut, &header, sizeof(WAVEHDR));
//                 waveOutWrite(hWaveOut, &header, sizeof(WAVEHDR));
//                 waveOutPause(hWaveOut);
                paused = 1;
            }
            else
            {
                if(channel == 0 && button == 0x07) fader0 = (double)b2/(double)0x7F;
                else if(channel == 1 && button == 0x07) fader1 = (double)b2/(double)0x7F;
                else if(channel == 2 && button == 0x07) fader2 = (double)b2/(double)0x7F;
                else if(channel == 3 && button == 0x07) fader3 = (double)b2/(double)0x7F;
                else if(channel == 4 && button == 0x07) fader4 = (double)b2/(double)0x7F;
                else if(channel == 5 && button == 0x07) fader5 = (double)b2/(double)0x7F;
                else if(channel == 6 && button == 0x07) fader6 = (double)b2/(double)0x7F;
                else if(channel == 7 && button == 0x07) fader7 = (double)b2/(double)0x7F;
                
                if(channel == 0 && b3hi == 0x01) dial0 = (double)b2/(double)0x7F; 
                if(channel == 1 && b3hi == 0x01) dial0 = (double)b2/(double)0x7F; 
                if(channel == 2 && b3hi == 0x01) dial0 = (double)b2/(double)0x7F; 
                if(channel == 3 && b3hi == 0x01) dial0 = (double)b2/(double)0x7F; 
                if(channel == 4 && b3hi == 0x01) dial0 = (double)b2/(double)0x7F; 
                if(channel == 5 && b3hi == 0x01) dial0 = (double)b2/(double)0x7F; 
                if(channel == 6 && b3hi == 0x01) dial0 = (double)b2/(double)0x7F; 
                if(channel == 7 && b3hi == 0x01) dial0 = (double)b2/(double)0x7F; 
            }
        }

        draw();
        
        printf("wMsg=MIM_DATA, dwParam1=%08x, byte=%02x %02x h_%01x l_%01x %02x, dwParam2=%08x\n", dwParam1, b1, b2, b3hi, b3lo, b4, dwParam2);
    }
    
	return;
}
#endif

void create_render_framebuffers()
{
    // Create framebuffer for rendering first pass to
    glGenFramebuffers(1, &first_pass_framebuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, first_pass_framebuffer);
    glGenTextures(1, &first_pass_texture);
    glBindTexture(GL_TEXTURE_2D, first_pass_texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, first_pass_texture, 0);
    glDrawBuffer(GL_COLOR_ATTACHMENT0);
}

// void load_compressed_sound()
// {
//     // Generate music framebuffer
//     // Initialize sequence texture
//     printf("sequence texture width is: %d\n", sequence_texture_size); // TODO: remove
//     glGenTextures(1, &sequence_texture_handle);
//     glBindTexture(GL_TEXTURE_2D, sequence_texture_handle);
//     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
//     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
//     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
//     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
//     glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, sequence_texture_size, sequence_texture_size, 0, GL_RGBA, GL_UNSIGNED_BYTE, sequence_texture);
// 
//     glGenFramebuffers(1, &snd_framebuffer);
//     glBindFramebuffer(GL_FRAMEBUFFER, snd_framebuffer);
//     glPixelStorei(GL_PACK_ALIGNMENT, 4);
//     glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
// 
//     unsigned int snd_texture;
//     glGenTextures(1, &snd_texture);
//     glBindTexture(GL_TEXTURE_2D, snd_texture);
//     glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, texs, texs, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0);
//     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
//     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
//     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
//     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
// 
//     glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, snd_texture, 0);
// 
//     // Music allocs
//     nblocks1 = sample_rate * duration1 / block_size + 1;
//     music1_size = nblocks1 * block_size;
//     smusic1 = (float*)malloc(4 * music1_size);
//     short *dest = (short*)smusic1;
//     for (int i = 0; i < 2 * music1_size; ++i)
//         dest[i] = 0;
// 
//     // Load music shader
//     int sfx_size = strlen(sfx_frag);
//     sfx_handle = glCreateShader(GL_FRAGMENT_SHADER);
//     sfx_program = glCreateProgram();
//     glShaderSource(sfx_handle, 1, (GLchar **)&sfx_frag, &sfx_size);
//     glCompileShader(sfx_handle);
//     printf("---> SFX shader:\n");
// #ifdef DEBUG
//     debug(sfx_handle);
// #endif
//     glAttachShader(sfx_program, sfx_handle);
//     glLinkProgram(sfx_program);
//     printf("---> SFX program:\n");
// #ifdef DEBUG
//     debugp(sfx_program);
// #endif
//     glUseProgram(sfx_program);
//     sfx_samplerate_location = glGetUniformLocation(sfx_program, SFX_VAR_ISAMPLERATE);
//     sfx_blockoffset_location = glGetUniformLocation(sfx_program, SFX_VAR_IBLOCKOFFSET);
//     sfx_volumelocation = glGetUniformLocation(sfx_program, SFX_VAR_IVOLUME);
//     sfx_texs_location = glGetUniformLocation(sfx_program, SFX_VAR_ITEXSIZE);
//     sfx_sequence_texture_location = glGetUniformLocation(sfx_program, SFX_VAR_ISEQUENCE);
//     sfx_sequence_width_location = glGetUniformLocation(sfx_program, SFX_VAR_ISEQUENCEWIDTH);
//     printf("++++ SFX shader created.\n");
// 
//     glBindFramebuffer(GL_FRAMEBUFFER, 0);
// 
//     progress += .1/NSHADERS;
// }

// void load_sound_block(int music_block)
// {
//     glBindFramebuffer(GL_FRAMEBUFFER, snd_framebuffer);
//     glUseProgram(sfx_program);
// 
//     printf("Rendering SFX block %d/%d -> %le\n", music_block, nblocks1, .5*(float)music_block / (float)nblocks1);
//     double tstart = (double)(music_block*block_size);
// 
//     glViewport(0, 0, texs, texs);
// 
//     glUniform1f(sfx_volumelocation, 1.);
//     glUniform1f(sfx_samplerate_location, (float)sample_rate);
//     glUniform1f(sfx_blockoffset_location, (float)tstart);
//     glUniform1f(sfx_texs_location, (float)texs);
//     glUniform1i(sfx_sequence_texture_location, 0);
//     glUniform1f(sfx_sequence_width_location, sequence_texture_size);
// 
//     glActiveTexture(GL_TEXTURE0);
//     glBindTexture(GL_TEXTURE_2D, sequence_texture_handle);
// 
//     quad();
// 
//     glReadPixels(0, 0, texs, texs, GL_RGBA, GL_UNSIGNED_BYTE, smusic1 + music_block * block_size);
//     glFlush();
// 
//     unsigned short *buf = (unsigned short*)smusic1;
//     short *dest = (short*)smusic1;
//     if (!muted)
//         for (int j = 2 * music_block*block_size; j < 2 * (music_block + 1)*block_size; ++j)
//             dest[j] = (buf[j] - (1 << 15));
//     else
//         for (int j = 2 * music_block*block_size; j < 2 * (music_block + 1)*block_size; ++j)
//             dest[j] = 0.;
// 
//     progress += .5/nblocks1;
// }

#include "engine/shader.h"
#include "shaders.gen.h"
void load_demo()
{
    printf("++++ Creating Loading bar.\n");
    lInitializeLoader();
#ifdef DEBUG_SHADER
    if (shader_program_gfx_load.linkStatus != GL_TRUE)
    {
        printf("    Linker Error. Log:\n%s\n\n", shader_program_gfx_load.linkerError);
    }
#endif

    printf("++++ Loading bar created.\n");

    create_render_framebuffers();

    updateBar();

    lLoadAllSymbols();
#ifdef DEBUG_SHADER
    for(unsigned int symbolIndex = 0; symbolIndex < lNumberOfSymbols; ++symbolIndex)
    {
        if (shader_symbols[symbolIndex].compileStatus != GL_TRUE)
        {
//             printf("    %s:\n", shader_symbols[symbolIndex].name
            printf("    Compiler Error. Log:\n%s\n\n", shader_symbols[symbolIndex].compilerError);
        }
    }
#endif

    lLoadAllPrograms();
#ifdef DEBUG_SHADER
    for (unsigned int programIndex = 0; programIndex < lNumberOfPrograms; ++programIndex)
    {
        if (shader_programs[programIndex].linkStatus != GL_TRUE)
        {
            printf("    Compiler Error. Log:\n%s\n\n", shader_programs[programIndex].linkerError);
        }
    }
#endif

    load_font();

    updateBar();

//     load_compressed_sound();
//     music_loading = 1;

//     updateBar();

//     for (int music_block = 0; music_block < nblocks1; ++music_block)
//     {
//         load_sound_block(music_block);
// 
//         updateBar();
// 	}

// 	glUseProgram(0);

// 	initialize_sound();
    
#ifdef MIDI
    UINT nMidiDeviceNum;
    MIDIINCAPS caps;
    
	nMidiDeviceNum = midiInGetNumDevs();
	if(nMidiDeviceNum == 0) 
    {
        printf("No MIDI input devices connected.\n");
    }
    else
    {
        printf("Available MIDI devices:\n");
        for (unsigned int i = 0; i < nMidiDeviceNum; ++i) 
        {
            midiInGetDevCaps(i, &caps, sizeof(MIDIINCAPS));
            printf("->%d: %s ", i, caps.szPname);
            
            if(!strcmp("APC40 mkII", caps.szPname))
            {
                HMIDIIN hMidiDevice;
                MMRESULT rv = midiInOpen(&hMidiDevice, i, (DWORD)(void*)MidiInProc_apc40mk2, 0, CALLBACK_FUNCTION);
                midiInStart(hMidiDevice);
                
                printf(" >> opened.\n");
            }
            else
            {
                printf("(Unsupported MIDI controller).\n");
            }
        }
    }
    
    MIDIOUTCAPS ocaps;
    nMidiDeviceNum = midiOutGetNumDevs();

    if(nMidiDeviceNum == 0) 
    {
        printf("No MIDI output devices connected.\n");
    }
    else
    {
        printf("Available MIDI devices:\n");
        for (unsigned int i = 0; i < nMidiDeviceNum; ++i) 
        {
            midiOutGetDevCaps(i, &ocaps, sizeof(MIDIOUTCAPS));
            printf("->%d: %s ", i, ocaps.szPname);
            
            if(!strcmp("APC40 mkII", ocaps.szPname))
            {
                MMRESULT rv = midiOutOpen (&hMidiOut, i, 0, 0, CALLBACK_NULL);
            }
            else
            {
                printf("(Unsupported MIDI controller).\n");
            }
        }
    }
    
    select_button(1);
#endif
}

void load_font()
{
    // Initialize font texture
    printf("font texture width is: %d\n", font_texture_size); // TODO: remove
    glGenTextures(1, &font_texture_handle);
    glBindTexture(GL_TEXTURE_2D, font_texture_handle);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, font_texture_size, font_texture_size, 0, GL_RGBA, GL_UNSIGNED_BYTE, font_texture);

    progress += .1/NSHADERS;
}

// Pure opengl drawing code, essentially cross-platform
void draw()
{
    glBindFramebuffer(GL_FRAMEBUFFER, first_pass_framebuffer);
    
    t = t_now;
//     printf("%le\n", t);
//     if(t > t_end)
//     {
// #ifdef RECORD
//         if(recording) capFileSaveAs(hCaptureWindow, record_filename); 
// #endif
//         ExitProcess(0);
//     }
    
// #ifdef MIDI
//     if(time_dial != 0 ||  time_fine_dial != 0 || time_very_fine_dial != 0)
//     {
//         t = t_now + (.9*time_dial+.09*time_fine_dial+.01*time_very_fine_dial) * duration;
//     }
// #endif
    
// #include "draw.h"
    int l = ARRAYSIZE(shader_programs);
    glUseProgram(shader_programs[override_index % l].handle);
    glUniform1f(shader_programs[override_index % l].uniforms[0].location, t);
    glUniform2f(shader_programs[override_index % l].uniforms[1].location, w, h);
    
    quad();

    // Render post processing to buffer
    glUseProgram(shader_program_gfx_post.handle);
    glUniform2f(shader_uniform_gfx_post_iResolution, w, h);
    glUniform1f(shader_uniform_gfx_post_iFSAA, fsaa);
    glUniform1i(shader_uniform_gfx_post_iChannel0, 0);
    glUniform1f(shader_uniform_gfx_post_iTime, t);
    
    glUniform1f(shader_uniform_gfx_post_iFader0, fader0);
    glUniform1f(shader_uniform_gfx_post_iFader1, fader1);
    glUniform1f(shader_uniform_gfx_post_iFader2, fader2);
    glUniform1f(shader_uniform_gfx_post_iFader3, fader3);
    glUniform1f(shader_uniform_gfx_post_iFader4, fader4);
    glUniform1f(shader_uniform_gfx_post_iFader5, fader5);
    glUniform1f(shader_uniform_gfx_post_iFader6, fader6);
    glUniform1f(shader_uniform_gfx_post_iFader7, fader7);
                                   
    glUniform1f(shader_uniform_gfx_post_iDial0, dial0);
    glUniform1f(shader_uniform_gfx_post_iDial1, dial1);
    glUniform1f(shader_uniform_gfx_post_iDial2, dial2);
    glUniform1f(shader_uniform_gfx_post_iDial3, dial3);
    glUniform1f(shader_uniform_gfx_post_iDial4, dial4);
    glUniform1f(shader_uniform_gfx_post_iDial5, dial5);
    glUniform1f(shader_uniform_gfx_post_iDial6, dial6);
    glUniform1f(shader_uniform_gfx_post_iDial7, dial7);
    
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, first_pass_texture);
//     glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0);

    quad();
    
    // Render to screen
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    
    glUseProgram(shader_program_gfx_text.handle);
    glUniform2f(shader_uniform_gfx_text_iResolution, w, h);
    glUniform1f(shader_uniform_gfx_text_iFontWidth, font_texture_size);
    glUniform1f(shader_uniform_gfx_text_iTime, t);
    glUniform1i(shader_uniform_gfx_text_iChannel0, 0);
    glUniform1i(shader_uniform_gfx_text_iFont, 1);
    glUniform1f(shader_uniform_gfx_text_iFSAA, fsaa);
    
#ifdef MIDI
    glUniform1f(shader_uniform_gfx_text_iFader0, fader0);
    glUniform1f(shader_uniform_gfx_text_iFader1, fader1);
    glUniform1f(shader_uniform_gfx_text_iFader2, fader2);
    glUniform1f(shader_uniform_gfx_text_iFader3, fader3);
    glUniform1f(shader_uniform_gfx_text_iFader4, fader4);
    glUniform1f(shader_uniform_gfx_text_iFader5, fader5);
    glUniform1f(shader_uniform_gfx_text_iFader6, fader6);
    glUniform1f(shader_uniform_gfx_text_iFader7, fader7);
    
    glUniform1f(shader_uniform_gfx_text_iDial0, dial0);
    glUniform1f(shader_uniform_gfx_text_iDial1, dial1);
    glUniform1f(shader_uniform_gfx_text_iDial2, dial2);
    glUniform1f(shader_uniform_gfx_text_iDial3, dial3);
    glUniform1f(shader_uniform_gfx_text_iDial4, dial4);
    glUniform1f(shader_uniform_gfx_text_iDial5, dial5);
    glUniform1f(shader_uniform_gfx_text_iDial6, dial6);
    glUniform1f(shader_uniform_gfx_text_iDial7, dial7);
#endif
    
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, first_pass_texture);
//     glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0);
    
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, font_texture_handle);
//     glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, font_texture_size, font_texture_size, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0);
    
    quad();
    glBindTexture(GL_TEXTURE_2D, 0);
}



