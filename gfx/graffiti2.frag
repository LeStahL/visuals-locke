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

// Global constants
const float pi = acos(-1.);
const vec3 c = vec3(1.0, 0.0, -1.0);
float a = 1.0;

float nbeats;

void rand(in vec2 x, out float n)
{
    x += 400.;
    n = fract(sin(dot(sign(x)*abs(x) ,vec2(12.9898,78.233)))*43758.5453);
}

void lfnoise(in vec2 t, out float n)
{
    vec2 i = floor(t);
    t = fract(t);
    t = smoothstep(c.yy, c.xx, t);
    vec2 v1, v2;
    rand(i, v1.x);
    rand(i+c.xy, v1.y);
    rand(i+c.yx, v2.x);
    rand(i+c.xx, v2.y);
    v1 = c.zz+2.*mix(v1, v2, t.y);
    n = mix(v1.x, v1.y, t.x);
}

void dlinesegment(in vec2 x, in vec2 p1, in vec2 p2, out float d)
{
    vec2 da = p2-p1;
    d = length(x-mix(p1, p2, clamp(dot(x-p1, da)/dot(da,da),0.,1.)));
}

//distance to spline with parameter t
float dist2(vec2 p0,vec2 p1,vec2 p2,vec2 x,float t)
{
    t = clamp(t, 0., 1.);
    return length(x-pow(1.-t,2.)*p0-2.*(1.-t)*t*p1-t*t*p2);
}

//minimum dist3ance to spline
void dspline2(in vec2 x, in vec2 p0, in vec2 p1, in vec2 p2, out float ds)
{
    //coefficients for 0 = t^3 + a * t^2 + b * t + c
    vec2 E = x-p0, F = p2-2.*p1+p0, G = p1-p0;
    vec3 ai = vec3(3.*dot(G,F), 2.*dot(G,G)-dot(E,F), -dot(E,G))/dot(F,F);

	//discriminant and helpers
    float tau = ai.x/3., p = ai.y-tau*ai.x, q = - tau*(tau*tau+p)+ai.z, dis = q*q/4.+p*p*p/27.;
    
    //triple real root
    if(dis > 0.) 
    {
        vec2 ki = -.5*q*c.xx+sqrt(dis)*c.xz, ui = sign(ki)*pow(abs(ki), c.xx/3.);
        ds = dist2(p0,p1,p2,x,ui.x+ui.y-tau);
        return;
    }
    
    //three dist3inct real roots
    float fac = sqrt(-4./3.*p), arg = acos(-.5*q*sqrt(-27./p/p/p))/3.;
    vec3 t = c.zxz*fac*cos(arg*c.xxx+c*pi/3.)-tau;
    ds = min(
        dist2(p0,p1,p2,x, t.x),
        min(
            dist2(p0,p1,p2,x,t.y),
            dist2(p0,p1,p2,x,t.z)
        )
    );
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

float sm(float d)
{
    return smoothstep(1.5/iResolution.y, -1.5/iResolution.y, d);
}

// iq's smooth minimum
void smoothmin(in float a, in float b, in float k, out float dst)
{
    float h = max( k-abs(a-b), 0.0 )/k;
    dst = min( a, b ) - h*h*h*k*(1.0/6.0);
}

float ind;
void graf(in vec2 x, in vec2 size, in float w, out float dst)
{
    x.x += mix(.1,1.,iFader7)*iTime;
    x.y -= .1;
    
    vec2 y = vec2(mod(x.x, size.x)-.5*size.x, x.y);
    ind = (y.x-x.x)/size.x;
    vec3 r,
        d;
    
    size.x -= 2.*w; 
    
    rand(vec2(ind,0.), r.x);
    rand(vec2(ind,1337.), r.y);
    rand(vec2(ind,2337.), r.z);
    
    vec2 pc = vec2(.5*size.x*(-1.+2.*r.y), 0.);
    
    // x component of r selects first actor
    if(r.x < .5) // Lin
    {
        vec2 p1 = vec2(.5*size.x*(-1.+4.*r.x),-.5*size.y),
            p2 = pc;
        dlinesegment(y, p1, p2, dst);
    }
    else // Quad
    {
        vec2 p1 = vec2(.5*size.x*(-1.+4.*(r.x-.5)),-.5*size.y),
            p2 = vec2(.5*size.x*(-1.+2.*r.y), -.5*size.y),
            p3 = pc;
        dspline2(y, p1, p2, p3, dst);
    }
    
    // z component of r selects second actor
    if(r.z < .5) // Lin
    {
        vec2 p1 = vec2(.5*size.x*(-1.+4.*r.z),.5*size.y),
            p2 = pc;
        dlinesegment(y, p1, p2, d.x);
    }
    else // Quad
    {
        vec2 p1 = vec2(.5*size.x*(-1.+4.*(r.z-.5)),.5*size.y),
            p2 = vec2(.5*size.x*(-1.+2.*r.y), .5*size.y),
            p3 = pc;
        dspline2(y, p1, p2, p3, d.x);
    }
    
    dst = min(dst, d.x);
    
    // Generate displacement
    lfnoise(12.*x, d.y);
    lfnoise(22.*x, d.z);
    d.y += .3*d.z;
    d.y = floor(.2+d.y);
    
    stroke(dst, w+.08*d.y, dst);
}

void add(in vec2 sda, in vec2 sdb, out vec2 sdf)
{
    sdf = mix(sda, sdb, step(sdb.x, sda.x));
}

void scene(in vec3 x, out vec2 sdf)
{
    float n;
    lfnoise(5.*x.xy+iTime*c.xy, n);
    x.z += .03*n;
    
    float d = 1., da;
    for(float i=0.; i<2.; i+=1.)
    {
    	graf(x.xy-1337.333*i*c.xy, vec2(.4,.6), .05, da);
        float rr;
        rand(i*c.xx*1.e2,rr); 
        zextrude(x.z, -d+.5*abs(x.z), .07+.06*rr, d);
        stroke(d,mix(.02,.04,iScale), d);
        smoothmin(d, da, .2, d);
    }
    sdf = vec2(d, 2.);
    
    add(sdf, vec2(x.z+.05,1.), sdf);
}     

void normal(in vec3 x, out vec3 n, in float dx)
{
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

void colorize(in vec2 x, out vec3 col)
{
    x.x += mix(.1,1.,iFader7)*iTime;
    x.y -= .05;
    
    col = .5*c.xxx;
    
    float s = .1;
    vec2 dd = mod(x, s)-.5*s;
    stroke(dd.x, .005, dd.x);
    stroke(dd.y, .005, dd.y);
    col = mix(col, c.xxx, sm(min(dd.x, dd.y)));
    
    float d = 1., da;
    for(float i=0.; i<2.; i+=1.)
    {
    	graf(x.xy-1337.333*i*c.xy-.3*iTime*c.xy, vec2(.4,.6), .05, da);
        float rr;
        rand(i*c.xx*1.e2,rr); 
        stroke(da,.18, da);
        smoothmin(d, da, .2, d);
    }
    vec3 c1 = vec3(.78,.61*abs(2.*x.y),.15);
    
    col = mix(col, c1, sm(d));
    
    if(d != 1.)
    {
        stroke(d-.03, .03, d);
        col = mix(col, c.yyy, sm(d));
    }
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    a = iResolution.x/iResolution.y;
    nbeats = mod(iTime, 60./29.);
    
    vec2 uv = fragCoord/iResolution.yy-0.5*vec2(a, 1.0), 
        s;
    vec3 col = c.yyy, 
        o = c.yzx,
        r = c.xyy, 
        u = normalize(c.yxx),
        t = c.yyy, 
        dir,
        n,
        x;
    int N = 150,
        i;
    t = uv.x * r + uv.y * u;
    dir = normalize(t-o);

    float d = -(o.z-.08)/dir.z;
    
    for(i = 0; i<N; ++i)
    {
     	x = o + d * dir;
        scene(x,s);
        if(s.x < 1.e-4)break;
        if(x.z<-.1)
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
        normal(x,n, 5.e-3);
        
        if(s.y == 1.)
        {
            vec3 l = normalize(x+.5*c.yzx);
            colorize(x.xy, col);
            col = .1*col
                + 1.*col * abs(dot(l,n))
                + 1.5 * col *  pow(abs(dot(reflect(x-l,n),dir)),3.);
            
        }
        else if(s.y == 2.)
        {
            vec3 l = normalize(x+c.xzx);
            float r;
            lfnoise(x.xy, r);
            col = vec3(0.99,0.43,0.15);
            
            vec3 c2 = vec3(0.44,0.07,0.66);
            
            col = mix(col,c2,sin(2.*iScale*r*x));
            col = .1*col
                + .8*col * abs(dot(l,n))
                + 6.5*col * pow(abs(dot(reflect(x-l,n),dir)),3.);
        }
    }
    
    col *= col;
    col = mix(col, c.yyy, clamp((d-2.-(o.z-.2)/dir.z)/4.,0.,1.));
    
    col = mix(c.yyy, col, smoothstep(0., 1., iTime));
    
    fragColor = vec4(clamp(col,0.,1.),1.0);
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy);
}
