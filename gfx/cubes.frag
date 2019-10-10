/* Gross Gloss by Team210 - 64k intro by Team210 at Solskogen 2k19
 * Copyright (C) 2019  Alexander Kraus <nr4@z10.info>
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

// Update1: Changes implementing FabriceNeyret2's comments.

// Global constants
const vec3 c = vec3(1.0, 0.0, -1.0);

void rand(in vec2 x, out float n)
{
    x += 400.;
    n = fract(sin(dot(sign(x)*abs(x) ,vec2(12.9898,78.233)))*43758.5453);
}

// Box sdf
void dbox(in vec2 x, in vec2 b, out float d)
{
    vec2 da = abs(x)-b;
    d = length(max(da,c.yy)) + min(max(da.x,da.y),0.0);
}

// Stroke
void stroke(in float d0, in float s, out float d)
{
    d = abs(d0)-s;
}

// Extrusion
void zextrude(in float z, in float d2d, in float h, out float d)
{
    vec2 w = vec2(-d2d, abs(z)-0.5*h);
    d = length(max(w,0.0));
}

// Add sdfs
void add(in vec2 sda, in vec2 sdb, out vec2 sdf)
{
    sdf = sda.x<sdb.x?sda:sdb;
}

vec2 ind;
void scene(in vec3 x, out vec2 sdf)
{
    x.y += .3*iTime;
    x.xy = vec2(x.x-x.y,x.x+x.y)/sqrt(2.);
    
    float d,
        size = .1;
    vec2 x2 = mod(x.xy,size)-.5*size;
	
    ind = (x.xy - x2)/size;
    dbox(x2, .5*size*c.xx, d);
    zextrude(x.z, -d-.005, .05, d);
    d = max(x.z,d);
    d = abs(d);
    sdf = vec2(d,2.);
    
    float r, r2;
    rand(ind-floor(iTime), r);
    rand(ind-floor(iTime)-1., r2);
    r = mix(r, r2, smoothstep(.8,1.,fract(iTime)));
    if(r > .7)
    {
        r2 = 1.;
        dbox(x2, .5*size*c.xx, d);
        zextrude(x.z, -d-.02, .3*(r-.7)/.3*r2, d);
        stroke(d, .001, d);
        add(sdf, vec2(d,1.), sdf);
    }
}

void normal(in vec3 x, out vec3 n)
{
    const float dx = 1.e-4;
    vec2 s, na;
    
    scene(x,s);
    scene(x+dx*c.xyy, na);
    n.x = na.x;
    scene(x+dx*c.yxy, na);
    n.y = na.x;
    scene(x+dx*c.yyx, na);
    n.z = na.x;
    n = normalize(n-s.x);
}

float sm(float d)
{
    return smoothstep(1.5/iResolution.y, -1.5/iResolution.y, d);
}

void colorize(in vec2 x, out vec3 col)
{
    x.y += .3*iTime;
    x = vec2(x.x-x.y,x.x+x.y)/sqrt(2.);
    
    float d,
        size = .1,
        r;
    vec2 x2 = mod(x.xy,size)-.5*size;
    
    float r2;
    rand(ind-floor(iTime), r);
    rand(ind-floor(iTime)-1., r2);
    r = mix(r, r2, smoothstep(.8,1.,fract(iTime)));
    col = mix(.14*c.xxx, .33*c.xxx, r);
    dbox(x2, .35*size*c.xx, d);
    if(r > .9)
    {
        col = mix(col, mix(c.xxy, c.xxx, .8), sm(d));
        stroke(d, .0025, d);
        col = mix(col, mix(c.xyy,c.xxx,.8), sm(d));
        stroke(d-.004, .002, d);
        col = mix(col, c.xyy, sm(d));
    }
	else if(r > .8)
    {
        col = mix(col, mix(c.xyy, c.xxx, .8), sm(d));
        stroke(d, .0025, d);
        col = mix(col, mix(.7*c.xxy,c.xxx,.8), sm(d));
        stroke(d-.004, .002, d);
        col = mix(col, .7*c.xxy, sm(d));
    }
    else if(r > .7)
    {
        col = mix(col, mix(c.xyy, c.xxx, .8), sm(d));
        stroke(d, .0025, d);
        col = mix(col, mix(mix(c.xxy, c.xyy, .5),c.xxx,.8), sm(d));
        stroke(d-.004, .002, d);
        col = mix(col, mix(c.xxy, c.xyy, .5), sm(d));
    }
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 uv = ( fragCoord -.5* iResolution.xy) / iResolution.y, 
        s;
    vec3 col = c.yyy, 
        o = c.yzx,
        r = c.xyy, 
        u = normalize(c.yxx), 
        t = c.yyy, 
        dir,
        n,
        x;
    int N = 100,
        i;
    t = uv.x * r + uv.y * u;
    dir = normalize(t-o);

    float d = -(o.z-.15)/dir.z;
    
    for(i = 0; i<N; ++i)
    {
     	x = o + d * dir;
        scene(x,s);
        if(s.x < 1.e-4)break;
        if(x.z<-.05)
        {
            col = .2*c.xxx;
            i = N;
            break;
        }
        d += min(s.x,5.e-3);
        //d += s.x;
    }
    
    if(i < N)
    {
        normal(x,n);
        
        if(s.y == 1.)
        {
            vec3 l = normalize(x+c.xzx);
            vec3 c1;
            
            float r;
		    rand(ind-floor(iTime), r);
            if(r > .9)
                col = c.xyy;
            else if(r > .8)
                col = .7*c.xxy;
            else if(r > .7)
                col = mix(c.xxy, c.xyy, .5);
            float sc = clamp((r-.7)/.3,0.,1.);
            col = mix(mix(col, c.xxx, .1*sc), .4*c.xyy, sc);
            col = .3*col
                + .9*col * abs(dot(l,n))
                + 1.3*col * pow(abs(dot(reflect(-l,n),dir)),3.);
            col = mix(col, c.xxx, .4);
            col *= col;
            
            d = -(o.z)/dir.z;
            x = o + d * dir;
            scene(x,s);
            l = normalize(x+c.xzx);
            colorize(x.xy, c1);
            n = c.yyx;
            
            c1 = .1*c1
                + .8*c1 * abs(dot(l,n))
                + c1 * pow(abs(dot(reflect(-l,n),dir)),3.);
            col = mix(col, c1, .3);
        }
        else if(s.y == 2.)
        {
            vec3 l = normalize(x+c.xzx);
            float r;
            
            colorize(x.xy, col);
            col = .1*col
                + .8*col * abs(dot(l,n))
                + col * pow(abs(dot(reflect(-l,n),dir)),3.);
        }
    }
    col += col;
    col *= col;
    
    fragColor = vec4(clamp(col,0.,1.),1.0);
}	

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy);
}
