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

const float iNote = 0.;

void rand(in vec2 x, out float num);
void zextrude(in float z, in float d2d, in float h, out float d);
void stroke(in float d0, in float s, out float d);
void smoothmin(in float a, in float b, in float k, out float dst);
void dhexagonpattern(in vec2 p, out float d, out vec2 ind);
void normal(in vec3 x, out vec3 n, in float dx);
void rot3(in vec3 p, out mat3 rot);
void lfnoise(in vec2 t, out float n);

float mat;
void scene(in vec3 x, out vec2 d)
{
    d = c.xx;
    
    x.z -= mix(1.,5.,iDial0)*iTime;
    
    float phi = atan(x.y, x.x),
        dhex,
        na,
        nal;
    vec2 ind;
    rand(floor(.33*iTime)*c.xx, na);
    rand(floor(.33*iTime)*c.xx+1., nal);
    na = mix(na,nal,clamp(((.33*iTime-floor(.33*iTime))-.9)/.1,0.,1.));
    dhexagonpattern(mix(1.,4.,na)*1.01*vec2(pi,3.)*vec2(phi,x.z),dhex,ind);
    rand(ind,mat);
    stroke(dhex, .1, dhex);
    mat *= (1.-iScale);
    //d.x = length(x.xy)-mix(.5,.45,smoothstep(.1,-.1,dhex));
    d.x = min(d.x, length(x.xy)+.2*mat-mix(.5,.55+.2*mat,smoothstep(.2,-.2,dhex)));
    
    stroke(d.x, .04, d.x);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    // Set up coordinates
    a = iResolution.x/iResolution.y;
    vec2 uv = fragCoord/iResolution.yy-0.5*vec2(a, 1.0);
    vec3 col = c.yyy;
    
    if(length(uv) > .5)
    {
        fragColor = vec4(col, 0.);
        return;
    }
    
    // Camera setup
    float pp = .3*iTime;
    vec3 o = c.yyx,
        t = c.yyy,
        dir = normalize(t-o),
        r = normalize(c.xyy),
        u = normalize(cross(r,dir)),
        n,
        x,
        l;
    t += uv.x*r + uv.y*u;
    dir = normalize(t-o);
    vec2 s;
    float d = (.3+.2*mat)/length(dir.xy);// -(o.z-.12)/dir.z;
    int N = 450,
        i;
    
    // Graph
    x = o + d * dir;
    
    // Actual Scene
    {

        // Raymarching
        for(i=0; i<N; ++i)
        {
            x = o + d * dir;
            scene(x,s);
            if(s.x < 1.e-4) break;
            d += min(s.x,.005);
        }

        // Illumination
        l = normalize(x+c.yxx);
        if(i<N)
        {
            normal(x,n,1.e-4);
                        
            float phi = atan(x.y, x.x),
                dhex,
                na,
                nal;
            vec2 ind;
            rand(floor(.33*iTime)*c.xx, na);
            rand(floor(.33*iTime)*c.xx+1., nal);
            na = mix(na,nal,clamp(((.33*iTime-floor(.33*iTime))-.9)/.1,0.,1.));
            
            mat3 RR;
            rot3(na*1.e3*vec3(1.1,1.5,1.9)+12.*iNote,RR);

            col = mix((.5+.5*mat)*c.xxx,(1.+.8*mat)*abs(RR*vec3(0.89,0.44,0.23)),.5+.5*sin(x.z));
            col = mix(col,vec3(0.25,0.23,0.21),.5+.5*cos(4.*x.z+mat));

            
            dhexagonpattern(mix(1.,4.,na)*1.01*vec2(pi,3.)*vec2(phi,x.z-iTime),dhex,ind);
            stroke(dhex, .3, dhex);
            col = mix(col, clamp(1.9*col,c.yyy,c.xxx), mat*smoothstep(1.5/iResolution.y, -1.5/iResolution.y, -dhex));
        }
    }
    
    // Colorize
    col = .2*col
        + .9*col*abs(dot(l,n))
        +.7*col*abs(pow(dot(reflect(-l,n),dir),3.));
    
    fragColor = clamp(vec4(col,1.0),0.,1.);
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy);
}
