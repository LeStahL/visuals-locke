/*
 * Beer Loading Bar
 * 
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
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
float a = 1.0, ry = 1.0;

void rand(in vec2 x, out float n);

void lfnoise(in vec2 t, out float n);

void mfnoise(in vec2 x, in float d, in float b, in float e, out float n);

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

void dlinesegment2(in vec2 x, in vec2 p1, in vec2 p2, out float d)
{
    vec2 da = p2-p1;
    d = length(x-mix(p1, p2, clamp(dot(x-p1, da)/dot(da,da),0.,1.)));
}

void dcircle(in vec2 x, in float R, out float d)
{
    d = length(x)-R;
}

// Stroke
void stroke(in float d0, in float s, out float d)
{
    d = abs(d0)-s;
}

void smoothmin(in float a, in float b, in float k, out float dst)
{
    float h = max( k-abs(a-b), 0.0 )/k;
    dst = min( a, b ) - h*h*h*k*(1.0/6.0);
}

void colorize(in vec2 x, inout vec3 col)
{
    vec2 dsx = vec2(x.x, x.y);
    vec3 bcol = vec3(0.99,0.63,0.11),
        gray = vec3(0.62,0.59,0.48),
        lightgray = vec3(0.83,0.82,0.77),
        beer = mix(vec3(0.99,0.80,0.00), vec3(0.97,0.65,0.09), (.5+.5*x.y)/.8),
        lightbeer = mix(vec3(1.00,0.87,0.07), vec3(1.00,0.98,0.76), (.5+.5*x.y)/.8);
    
    // bubbles
    float dc = 1.;
    for(int i=0; i<200; ++i)
    {
        float index = float(i);
        vec2 dx;
        rand(index*c.xx, dx.x);
        rand(index*c.xx-1338., dx.y);
        float dd;
        dcircle(mod(.5+x-vec2(.2,.5)*mix(-c.xx,c.xx,dx)-1.*iFader7*c.yx,1.)-.5, .03*dx.x, dd);
        
        smoothmin(dc, dd, .02, dc);
    }
    float ddc;
    stroke(dc, .002, ddc);
    beer = mix(beer, vec3(0.73,0.47,0.00), smoothstep(ry, -ry, ddc));
    beer = mix(beer, vec3(0.98,0.78,0.07), smoothstep(ry, -ry, dc+.005));
    
    float d, da, db, dhandle, dbeer;
    dlinesegment2(vec2(.5,1.)*x, .25*c.yz, .25*c.yx, d);
    stroke(d, .1, d);
    dspline2(x, vec2(.2,-.15), vec2(.35,-.1)+.1*c.yz, vec2(.35,.05), da);
    dspline2(x, vec2(.35,.05), vec2(.35, .15), vec2(.2,.15), db);
    da = min(da, db);
    dhandle = da;
    stroke(da, .04, da);
  	smoothmin(d, da, .05, d);
    col = mix(col, bcol, smoothstep(ry, -ry, d));
    stroke(dhandle, .03, da);
    col = mix(col, gray, smoothstep(ry, -ry, d+.01));
    stroke(dhandle, .025, da);
    col = mix(col, lightgray, smoothstep(ry, -ry, da));
    stroke(dhandle, .005, da);
    col = mix(col, c.xxx, smoothstep(ry, -ry, da));
    
    dspline2(x, vec2(-.15,-.27), vec2(0.,-.36), vec2(.15,-.27), da);
    
    dlinesegment2(vec2(.5,1.)*x, .25*c.yz, .25*c.yx, dbeer);
    stroke(dbeer, .095, dbeer);
    col = mix(col, beer, smoothstep(ry,-ry,dbeer));
    
    // stripes
    dlinesegment2(vec2(.5,1.)*vec2(abs(x.x-.03)-.1,x.y)-.4*abs(x.x)*c.yx, -.3*c.yx, .25*c.yx, dbeer);
    stroke(dbeer, .015, dbeer);
    col = mix(col, lightbeer, smoothstep(ry,-ry,dbeer));
    
    stroke(da, .02, db);
    col = mix(col, lightgray, smoothstep(ry, -ry, db));
    
    dlinesegment2(vec2(.5,1.)*x, .2*c.yx-.4*iFader7*c.yx, .25*c.yx, dbeer);
    stroke(dbeer, .095, dbeer);
    col = mix(col, vec3(0.78,0.44,0.05), smoothstep(ry,-ry,dbeer));
    
    dlinesegment2(vec2(.5,1.)*x, .2*c.yx-.4*iFader7*c.yx, .25*c.yx, dbeer);
    stroke(dbeer, .092, dbeer);
    col = mix(col, vec3(0.95,0.94,0.87), smoothstep(ry,-ry,dbeer));
    
    // stripes
    dlinesegment2(vec2(.5,1.)*vec2(abs(x.x-.03)-.1,x.y)-.4*abs(x.x)*c.yx, .12*c.yx-.4*iFader7*c.yx, .25*c.yx, dbeer);
    stroke(dbeer, .015, dbeer);
    col = mix(col, vec3(1.00,1.00,1.00), smoothstep(ry,-ry,dbeer));
    
    // Foam
    dlinesegment2(vec2(.5,1.)*x, .3*c.yx, .3*c.yx, dbeer);
    dlinesegment2(x, vec2(-.2,.3), vec2(-.2,.1), da);
    stroke(da, .06, da);
    stroke(dbeer, .095, dbeer);
    smoothmin(dbeer, da, .05, dbeer);
    float n;
    mfnoise(x-.1*iTime,8.,800.,.35,n);
    dbeer += .03*mix(-1.,1.,n);
    col = mix(col, vec3(0.78,0.76,0.69), smoothstep(ry,-ry,dbeer));
    dbeer += .01*x.y;
    col = mix(col, vec3(0.88,0.88,0.81), smoothstep(ry,-ry,dbeer));
    dbeer += .05*x.y;
    col = mix(col, vec3(0.95,0.95,0.89), smoothstep(ry,-ry,dbeer));
    dbeer += .1*x.y;
    col = mix(col, vec3(1.00,1.00,0.95), smoothstep(ry,-ry,dbeer));
    
    col = mix(col, col*col, iScale);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
  	a = iResolution.x/iResolution.y;
  	ry = 1.5/iResolution.y;
  	vec2 uv = fragCoord/iResolution.yy-0.5*vec2(a, 1.0);
  	vec3 col = c.yyy;
  
//     iFader7 = fract(iTime);
    
  	colorize(uv,col);
  
  	fragColor = vec4(col, 1.);
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy);
}
