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


/* Diamond Tricks similiar to scene in PC-64k intro 'Hardcyber' by Team210 at Deadline 2k19
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

// Global constants
const float pi = acos(-1.);
const vec3 c = vec3(1.0, 0.0, -1.0);
float a = 1.0;
const float ra = .5;

void rand(in vec2 p, out float d);
void hash22(in vec2 p, out vec2 d);
void hash12(in vec2 p, out float d);
void lfnoise(in vec2 t, out float n);
void dbox3(in vec3 x, in vec3 b, out float d);
void add(in vec2 sda, in vec2 sdb, out vec2 sdf);
void sub(in vec2 sda, in vec2 sdb, out vec2 sdf);
void dvoronoi(in vec2 x, out float d, out vec2 p, out float control_distance);
void rot3(in vec3 p, out mat3 rot);
mat3 gR;
vec2 ind = c.yy;
void scene(in vec3 x, out vec2 sdf)
{
    vec3 y = x;
    x.xy += vec2(cos(.3*iTime), sin(.3*iTime));
    
    sdf.x = x.z;
    sdf.y = 0.;
    
    float db = abs(length(y-.1*c.yyx)-mix(.1,.2,iScale)), 
        dc;
    dbox3(gR * (y-.1*c.yyx), mix(.1,.2,iScale)*c.xxx/sqrt(2.), dc);
    db = mix(db, abs(dc)-.001,step(iFader6,.5));
    add(sdf, vec2(db,1.), sdf);
}

void texture_scene(in vec3 x, out vec2 sdf)
{
    vec3 y = x;
    x.xy += vec2(cos(.3*iTime), sin(.3*iTime));
    
    sdf.x = x.z;
    sdf.y = 0.;
    
    float res = 8.;
    vec2 sdb = c.xy;
    for(float f = 0.; f < 6.; f += 1.)
    {
        float v, vp;
        vec2 vi;
        dvoronoi(res*x.xy, v, vi, vp);
        vp /= res;
        vi /= res;
        v /= res;
        add(sdb, vec2(length(x-vec3(vi,0.))-.5*vp, 0.), sdb);
        res *= 2.;
        
        ind += vi/res;
    }
    sub(sdf, sdb, sdf);
    
    float db = abs(length(y-.1*c.yyx)-.2), 
        dc;
    dbox3(gR * (y-.1*c.yyx), .2*c.xxx/sqrt(2.), dc);
    db = mix(db, abs(dc)-.001, step(iFader6,.5));
    add(sdf, vec2(db,1.), sdf);
}

void normal(in vec3 x, out vec3 n, in float dx);

void texture_normal(in vec3 x, out vec3 n, in float dx)
{
    vec2 s, na;
    
    texture_scene(x,s);
    texture_scene(x+dx*c.xyy, na);
    n.x = na.x;
    texture_scene(x+dx*c.yxy, na);
    n.y = na.x;
    texture_scene(x+dx*c.yyx, na);
    n.z = na.x;
    n = normalize(n-s.x);
}

void palette(in float scale, out vec3 col)
{
    scale = clamp(scale, 1.e-2,.99);
    const int N = 5;
    vec3 colors[N] = vec3[N](
mix(vec3(0.20,0.27,0.35),vec3(1.00,0.00,0.47), step(iFader6,.5)),
mix(vec3(0.29,0.37,0.45),vec3(0.80,0.00,0.47), step(iFader6,.5)),
mix(vec3(0.36,0.65,0.64),vec3(0.60,0.00,0.47), step(iFader6,.5)),
mix(vec3(0.66,0.85,0.80),vec3(0.40,0.00,0.47), step(iFader6,.5)),
mix(vec3(0.95,0.92,0.82),c.yyy,step(fract(.5*iTime),.25))
    );
	float index = floor(scale*float(N)), 
        remainder = scale*float(N)-index;
    col = mix(colors[int(index)],colors[int(index)+1], remainder);
}

float sm(in float d)
{
    return smoothstep(1.5/iResolution.y, -1.5/iResolution.y, d);
}

void analytical_box(in vec3 o, in vec3 dir, in vec3 size, out vec2 d)
{
    vec3 tlo = min((size-o)/dir,(-size-o)/dir),
        thi = max((size-o)/dir,(-size-o)/dir);
    vec2 abxlo = abs(o.yz + tlo.x*dir.yz),
        abylo = abs(o.xz + tlo.y*dir.xz),
        abzlo = abs(o.xy + tlo.z*dir.xy),
        abxhi = abs(o.yz + thi.x*dir.yz),
        abyhi = abs(o.xz + thi.y*dir.xz),
        abzhi = abs(o.xy + thi.z*dir.xy);
    vec4 dn = 1.e4*c.xyyy;
    
    dn = mix(dn, vec4(tlo.x,c.xyy), float(all(lessThan(abxlo,size.yz)))*step(tlo.x,dn.x));
    dn = mix(dn, vec4(tlo.y,c.yxy), float(all(lessThan(abylo,size.xz)))*step(tlo.y,dn.x));
    dn = mix(dn, vec4(tlo.z,c.yyx), float(all(lessThan(abzlo,size.xy)))*step(tlo.z,dn.x));

    d.x = dn.r;
    
    dn = 1.e4*c.xyyy;
    dn = mix(dn, vec4(thi.x,c.xyy), float(all(lessThan(abxhi,size.yz)))*step(thi.x,dn.x));
    dn = mix(dn, vec4(thi.y,c.yxy), float(all(lessThan(abyhi,size.xz)))*step(thi.y,dn.x));
    dn = mix(dn, vec4(thi.z,c.yyx), float(all(lessThan(abzhi,size.xy)))*step(thi.z,dn.x));
    
    d.y = dn.r;
}

void analytical_sphere(in vec3 o, in vec3 dir, in float R, out vec2 d)
{
    float a = dot(dir,dir),
        b = 2.*dot(o,dir),
        cc = dot(o,o)-R*R,
        dis = b*b-4.*a*cc;
    vec2 dd = (dis<0.)?1.e4*c.xx:(c.xz*sqrt(dis)-b)/2./a;
    d = vec2(min(dd.x, dd.y), max(dd.x, dd.y));
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    // Set up global variables
    rot3(mix(.1,1.,iFader7)*vec3(1.1,1.3,1.5)*iTime, gR);
    
    // Set up coordinates and camera
    vec2 uv = (fragCoord.xy-.5*iResolution.xy)/iResolution.y,
        s,
        dd;
    uv *= .5;
    vec3 col = c.yyy,
        o = c.yyx+.4*c.yzy,
        r = c.xyy,
        t = c.yyy, 
        u = cross(normalize(t-o),-r),
        dir,
        n, 
        x,
        c1 = c.yyy,
        l;
    int N = 150,
        i;
    float d;
    
    t = uv.x * r + uv.y * u;
    dir = normalize(t-o);

    // Bounding objects
    if(iFader6 > .5)
        analytical_sphere(o-.1*c.yyx, dir, mix(.1,.2,iScale), dd);
    else
	    analytical_box(gR*(o-.1*c.yyx), gR*dir, mix(.1,.2,iScale)*c.xxx/sqrt(2.), dd);
    d = dd.x;
    
    if(d>1.e1) // Ray intersects outside of the refracting object
    {
        d = -o.z/dir.z;
        col = c.xxx;
    }
    else // Ray intersects the refracting object
    {
        x = o + d * dir;
        normal(x, n, 1.e-5);
        
        // Outside color
        l = x+c.yxy+.5*c.yyx;
        col = 3.*c.xxx;
        col = .3*col 
       	 	+ .5*col*dot(-l, n)
        	+ 3.7*col*pow(abs(dot(reflect(l,n),dir)),2.);
        
        dir = refract(dir, n, ra);
        o = x;
        
        // Bounding objects again, but from the inside
        if(iFader6 > .5)
            analytical_sphere(o-.1*c.yyx, dir, mix(.1,.2,iScale), dd);
        else
            analytical_box(gR*(o-.1*c.yyx), gR*dir, mix(.1,.2,iScale)*c.xxx/sqrt(2.), dd);
        d = dd.y;
        
        x = o + d * dir;
        normal(x, n, 1.e-5);
        
        // Inside color
        l = x+c.yxy+.5*c.yyx;
        c1 = c.xxx;
        c1 = .3*c1 
       	 	+ .5*c1*dot(-l, n)
        	+ 1.7*c1*pow(abs(dot(reflect(l,n),dir)),2.);
        col = mix(col, c1, .7);
        
        dir = refract(dir, n, ra);
        o = x;
        
        d = -o.z/dir.z;
        x = o + d * dir;
        normal(x, n, 1.e-5);
    }
    
    // Raymarch texture
    for(i = 0; i<N; ++i)
    {
        x = o + d * dir;
        texture_scene(x,s);
        if(s.x < 1.e-5)break;
        d += s.x;
    }
     
    // Colorize texture
	l = x+c.yxy+.5*c.yyx;
    texture_normal(x,n, 2.e-5);
        
    float na;
    lfnoise(ind-iTime, na);
    palette(.5+.5*na, c1);
        
    c1 = .3*c1 
        + .5*c1*dot(-l, n)
        + .7*c1*pow(abs(dot(reflect(l,n),dir)),2.);
    col = mix(col, c1, .9);
    
    // Post process
    float da = 1.;

    scene(vec3(uv,.1), s);
    da = abs(s.x)-.003;

    col = mix(col, 8.*col, sm(da/10.));
    col = 2.*col*col*col;
    
    fragColor = vec4(clamp(col,0.,1.),1.);
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy);
}
