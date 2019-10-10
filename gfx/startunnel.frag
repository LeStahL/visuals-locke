/* Endeavor by Team210 - 64k intro by Team210 at Revision 2k19
* Copyright (C) 2018  Alexander Kraus <nr4@z10.info>
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

#version 130

uniform float iTime;
uniform vec2 iResolution;
uniform float iScale;

uniform float iFader0;
uniform float iFader1;
uniform float iFader2;
uniform float iFader3;
uniform float iFader4;
uniform float iFader5;
uniform float iFader6;
uniform float iFader7;

uniform float iDial0;
uniform float iDial1;
uniform float iDial2;
uniform float iDial3;
uniform float iDial4;
uniform float iDial5;
uniform float iDial6;
uniform float iDial7;

// Global constants
const float pi = acos(-1.);
const vec3 c = vec3(1.0, 0.0, -1.0);
float a = 1.0;

void rand(in vec2 x, out float num);
void lfnoise(in vec2 t, out float num);
void mfnoise(in vec2 x, in float fmin, in float fmax, in float alpha, out float num);
void dbox(in vec2 p, in vec2 b, out float dst);
void dpolygon(in vec2 x, in float N, in float R, out float dst)
{
    float d = 2.*pi/N,
        t = mod(acos(x.x/length(x)), d)-.5*d;
    dst = R-length(x)*cos(t)/cos(.5*d);
}
void dstar(in vec2 x, in float N, in vec2 R, out float dst);
void rot3(in vec3 p, out mat3 rot);
void stroke(in float d0, in float s, out float d);

// Mix appropriate marble colors.
void color(in float scale, out vec3 col)
{
    const int N = 13;
    const vec3 colors[N] = vec3[N](
        c.yyy,
        vec3(0.15,0.14,0.12),
        vec3(0.38,0.16,0.16),
        vec3(0.42,0.20,0.19),
        vec3(0.60,0.14,0.16),
        vec3(0.70,0.11,0.15),
        vec3(0.89,0.11,0.10),
        vec3(0.89,0.27,0.03),
        vec3(0.92,0.39,0.14),
        vec3(0.91,0.47,0.15),
        vec3(0.92,0.57,0.14),
        vec3(0.90,0.63,0.12),
        vec3(0.92,0.72,0.14)
    );
	float index = floor(scale*float(N)), 
        remainder = scale*float(N)-index;
    col = mix(colors[int(index)],colors[int(index)+1], remainder);
}

void colorize(in vec2 uv, out vec3 col, float i)
{
    vec2 n, n2;
    lfnoise(i*c.xx-2.*iTime, n.x);
    lfnoise(i*c.xx-2.*iTime-1337., n.y);
    
    uv += i*.5*n;
    
    float ca = .5*i-3.5*iTime,
        cc = cos(ca),
        sc = sin(ca);
    mat2 RR = mat2(cc,sc,-sc,cc);
    uv = RR*uv;
    
    float dd;
    rand(floor(.33*iTime)*c.xx,dd);
    
    vec3 c1;
    color(clamp(i+2.*n.y,0.,1.), c1);
    
    float d, da, db;
    dpolygon(uv, max(ceil(8.*dd),3.), .4+0.*.4*iScale, d);
    dstar(uv,max(ceil(8.*dd),3.),vec2(.05,.5)+0.*vec2(.1,.4)*iScale,db);
    d = mix(d,db,.5+.5*n.y);
    stroke(d, .01, da);
    da -= .01*n.y;
    d -= .01*n.x;

        float mat = n.y;
//     rand(i*c.xx, mat);
        float phi = atan(uv.y, uv.x),
                dhex,
                na,
                nal;
            vec2 ind;
            rand(floor(.33*iTime)*c.xx, na);
            rand(floor(.33*iTime)*c.xx+1., nal);
            na = mix(na,nal,clamp(((.33*iTime-floor(.33*iTime))-.9)/.1,0.,1.));
            
            mat3 RRR;
            rot3(na*1.e3*vec3(1.1,1.5,1.9),RRR);

            c1 = mix((.5+.5*n.y)*c1,(1.+.8*mat)*abs(RRR*c1),.5+.5*n.x);
            c1 = mix(c1,.2*c1,.5+.5*n.y);

    
    col = mix(c1,c.yyy,smoothstep(1.5/iResolution.y, -1.5/iResolution.y,-da));
    col = mix(col, mix(col,c1,.03+.02*clamp(iScale,0.,1.)), smoothstep(1.5/iResolution.y, -1.5/iResolution.y,-d));
//     col = mix(col, mix(col,1.4*c1,.03+.02*clamp(iScale,0.,1.)), smoothstep(1.5/iResolution.y, -1.5/iResolution.y,abs(d)-.05));
    col = mix(col,(.5+iScale)*col,smoothstep(1.5/iResolution.y, -1.5/iResolution.y,-abs(da)+.005));
    
    col = clamp(col*1.3, 0.,1.);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    /// Set up coordinates
    a = iResolution.x/iResolution.y;
    vec2 uv = fragCoord/iResolution.yy-0.5*vec2(a, 1.0);
    vec3 col = c.yyy;
    
//     if(length(uv) > .5)
//     {
//         fragColor = vec4(col, 0.);
//         return;
//     }
    
    vec3 x,
        o = c.yyx,
        t = vec3(uv,0.),
        dir = normalize(t-o),
        c1;
    float d = .5;
    vec2 n;
    int N = 100,
        i;
    
    for(i=0; i<N; ++i)
    {
        d = -(o.z-.5+.1*float(i))/dir.z;
        x = o + d * dir;
           
        colorize(x.xy,c1, float(i)*mix(.01,.05,iScale));
        col += c1;
    }
    c1 /= float(N);
    
    fragColor = vec4(col,1.0);
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy);
}
