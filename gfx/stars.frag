#version 450

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

out vec4 out_color;

/*
 * Star Stacks
 * 
 * Copyright (C) 2021  Alexander Kraus <nr4@z10.info>
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
 
const vec3 c = vec3(1.,0.,-1.);
const float pi = 3.14159,
    PHI = 1.618,
    bpm = .5*149.,
    spb =  60. / bpm,
    minimalTimeStep = spb/8.;
mat3 RR = mat3(1.),
    RRA = mat3(1.);
float scale,
    nbeats;
const float tmax = 80.521;

// iq's smoothmin
float smoothmin(float a, float b, float k)
{
    float h = max( k-abs(a-b), 0.0 )/k;
    return min( a, b ) - h*h*h*k*(1.0/6.0);
}

float smoothmax(float a, float b, float k)
{
    return a + b - smoothmin(a,b,k);
}

float zextrude(float z, float d2d, float h)
{
    vec2 w = vec2(d2d, abs(z)-0.5*h);
    return min(max(w.x,w.y),0.0) + length(max(w,0.0));
}

// iq's hexagon pattern
void dhexagonpattern(in vec2 p, out float d, out vec2 ind) 
{
    vec2 q = vec2( p.x*1.2, p.y + p.x*0.6 );
    
    vec2 pi = floor(q);
    vec2 pf = fract(q);

    float v = mod(pi.x + pi.y, 3.0);

    float ca = step(1.,v);
    float cb = step(2.,v);
    vec2  ma = step(pf.xy,pf.yx);
    
    d = dot( ma, 1.0-pf.yx + ca*(pf.x+pf.y-1.0) + cb*(pf.yx-2.0*pf.xy) );
    ind = pi + ca - cb*ma;
    ind = vec2(ind.x/1.2, ind.y);
    ind = vec2(ind.x, ind.y-ind.x*.6);
}

mat3 rot3(in vec3 p)
{
    return mat3(c.xyyy, cos(p.x), sin(p.x), 0., -sin(p.x), cos(p.x))
        *mat3(cos(p.y), 0., -sin(p.y), c.yxy, sin(p.y), 0., cos(p.y))
        *mat3(cos(p.z), -sin(p.z), 0., sin(p.z), cos(p.z), c.yyyx);
}

// Creative Commons Attribution-ShareAlike 4.0 International Public License
// Created by David Hoskins.
// See https://www.shadertoy.com/view/4djSRW
float hash12(vec2 p)
{
	vec3 p3  = fract(vec3(p.xyx) * .1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

// Creative Commons Attribution-ShareAlike 4.0 International Public License
// Created by David Hoskins.
// See https://www.shadertoy.com/view/4djSRW
vec2 hash22(vec2 p)
{
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}

float lfnoise(vec2 t)
{
    vec2 i = floor(t);
    t = fract(t);
    t = smoothstep(c.yy, c.xx, t);
    vec2 v1 = vec2(hash12(i), hash12(i+c.xy)),
        v2 = vec2(hash12(i+c.yx), hash12(i+c.xx));
    v1 = c.zz+2.*mix(v1, v2, t.y);
    return mix(v1.x, v1.y, t.x);
}

float mfnoise(vec2 x, float d, float b, float e)
{
    float n = 0.;
    float a = 1., nf = 0., buf;
    for(float f = d; f<b; f *= 2.)
    {
        n += a*lfnoise(f*x-2.*iTime);
        a *= e;
        nf += 1.;
    }
    return n * (1.-e)/(1.-pow(e, nf));
}

vec3 hsv2rgb(vec3 cc)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(cc.xxx + K.xyz) * 6.0 - K.www);
    return cc.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), cc.y);
}

vec3 rgb2hsv(vec3 cc)
{
    vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4 p = mix(vec4(cc.bg, K.wz), vec4(cc.gb, K.xy), step(cc.b, cc.g));
    vec4 q = mix(vec4(p.xyw, cc.r), vec4(cc.r, p.yzx), step(p.x, cc.r));

    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

// Distance to line segment
float linesegment(in vec2 x, in vec2 p1, in vec2 p2)
{
    vec2 da = p2-p1;
    return length(x-mix(p1, p2, clamp(dot(x-p1, da)/dot(da,da),0.,1.)));
}

// Distance to star
float star(in vec2 x, in float r1, in float r2, in float N)
{
    N *= 2.;
    float p = atan(x.y,x.x),
        k = pi/N,
    	dp = mod(p+pi, 2.*k),
    	parity = mod(round((p+pi-dp)*.5/k), 2.),
        dk = k,
        dkp = mix(dk,-dk,parity);
    
    vec2 p1 = r1*vec2(cos(k-dkp),sin(k-dkp)),
        p2 = r2*vec2(cos(k+dkp),sin(k+dkp)),
        dpp = p2-p1,
        n = normalize(p2-p1).yx*c.xz, 
        xp = length(x)*vec2(cos(dp), sin(dp));
    float t = dot(xp-p1,dpp)/dot(dpp,dpp);
    float r = mix(1.,-1.,parity)*dot(xp-p1,n);
    if(t < 0.)
        return sign(r)*length(xp-p1);
    else if(t > 1.)
        return sign(r)*length(xp-p2);
    else
	    return r;
}

// Scene marching information
struct SceneData
{
    float

        // Material for palette
        material,
    
        // Distance
        dist,
    
        // Light accumulation for clouds
        accumulation,
    
        // Reflectivity
        reflectivity,
    
        // Transmittivity
        transmittivity,
    
        // Illumination
        specular,
    
        // Diffuse
        diffuse;
};

SceneData defaultMaterial(float d)
{
    return SceneData(1.3, d, 1., .1, .1, .5, 1.);
}

SceneData add(SceneData a, SceneData b)
{
    if(a.dist < b.dist) return a;
    return b;
}

float rj;

float holeSDF(vec3 x, float zj)
{
    float r = lfnoise(.5*nbeats*c.xx-zj),
        s = lfnoise(.5*nbeats*c.xx+1337.-zj);
        
    // star effect
    float ag = mix(2.,12.,.5+.5*r)*zj*r;
    mat2 RB = mat2(cos(ag), sin(ag), -sin(ag), cos(ag));
    float da = -abs(star(RB*(x.xy-vec2(r,s)*.5), abs(1.*r+.1*zj), abs(1.*s-.1*zj), round(5.+r+s)))+.05-.1*zj,
        db = (mod(da, .2))-mix(.09, .05, iScale)*2.1;
    rj = da - db;
    
    return db;
}

SceneData scene(vec3 x)
{
    SceneData sdf = SceneData(0., x.z+.5, 0., 0., 0., .7, 1.);

    float dz = .03,
        z = mod(x.z, dz) - .5 * dz,
        zj = x.z - z,
        zjz = zj / dz;

    if(zj <= 0.)
    {
        float d = zextrude(z, -holeSDF(x, zj), .5*dz)-.15*dz;
        sdf = add(
            sdf,
            SceneData(-1.+3.*abs(zjz/.5*dz), d, 0., 0., 0., .7, 1.)
        );
    }

    return sdf;
}

vec3 normal(vec3 x)
{
    float s = scene(x).dist,
        dx = 5.e-5;
    return normalize(vec3(
        scene(x+dx*c.xyy).dist, 
        scene(x+dx*c.yxy).dist, 
        scene(x+dx*c.yyx).dist
    )-s);
}

vec3 palette(float scale)
{
    const int N = 4;
    vec3 colors[N] = vec3[N](
        vec3(1.00,0.22,0.30),
        c.yyy,
        vec3(0.13,0.44,0.66),
        vec3(0.00,0.80,0.73)
    );
    float i = mod(floor(scale), float(N)),
        ip1 = mod(i + 1., float(N));
    return mix(colors[int(i)], colors[int(ip1)], fract(scale));
}

bool ray(out vec3 col, out vec3 x, inout float d, vec3 dir, out SceneData s, vec3 o, vec3 l, out vec3 n)
{
    for(int i=0; i<250; ++i)
    {
        x = o + d * dir;
        s = scene(x);
        
        if(s.dist < 1.e-4) 
        {
            // Blinn-Phong Illumination
            n = normal(x);

            if(s.material == 0.)
            {
                col = c.yyy;
            }
            else 
            {
                col = palette(s.material+rj*10. - length(x.xy));
            }

            col = .2 * col
                + s.diffuse * col*max(dot(normalize(l-x),n),0.)
                + s.specular * col*pow(max(dot(reflect(normalize(l-x),n),dir),0.),2.);

            return true;
        }
        
        d += min(s.dist,s.dist>1.e0?1.e-2:5.e-3);
    }
    return false;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    // Rotation tools
    RR = rot3(iTime*vec3(0.,0.,.6));
    RRA = rot3(iTime*vec3(.7,.9,1.32));

    // Sync tools
    float stepTime = mod(iTime, spb)-.5*spb;
    nbeats = mix(.5, 1.5, iFader7)*iTime;
    // scale = smoothstep(-.3*spb, 0., stepTime)*smoothstep(.3*spb, 0., stepTime);
	scale = iScale;

    // Marching tools
    float d = 0.,
        d1;
    vec2 uv = (fragCoord.xy-.5*iResolution.xy)/iResolution.y;
    vec3 o = RR*c.yzx,
        col = c.yyy,
        c1 = c.yyy,
        x,
        x1,
        n,
        n1,
        r = RR*c.xyy,
        t = c.yyy,
        dir = normalize(uv.x * r + uv.y * cross(r,normalize(t-o))-o),
        l = c.zzx;
    SceneData s, 
        s1;

    d = -(o.z)/dir.z;
    x = o + d * dir;
        
    // Material ray
    if(ray(col, x, d, dir, s, o, l, n))
    {
        s1 = s;
        d1 = d;
        n1 = n;
        
        // Ambient occlusion from iq
        float occ = 0.;
        for(int i=0; i<32; ++i)
        {
            float h = .01 + 4.0*pow(float(i)/31.0,2.0);
            vec2 an = hash22( hash12(iTime*c.xx)*c.xx + float(i)*13.1 )*vec2( 3.14159, 6.2831 );
            vec3 dir2 = vec3( sin(an.x)*sin(an.y), sin(an.x)*cos(an.y), cos(an.x) );
            dir2 *= sign( dot(dir2,n) );
            occ += clamp( 5.0*scene( x + h*dir2 ).dist/h, -1.0, 1.0);
        }
        col = mix(.5*col, col, clamp(occ/32.,0.,1.));
        
        // Soft shadow from iq
        if(x.z <= .0)
        {
            // Soft Shadow
            o = x;
            dir = normalize(l-x);
            d1 = 1.e-2;
            
            {
                float res = 1.0;
                float ph = 1.e20;
                for(int i=0; i<150; ++i)
                {
                    x = o + d1 * dir;
                    s = scene(x);
                    if(s.dist < 1.e-4) 
                    {
                        res = 0.;
                        break;
                    }
                    if(x.z >= .0)
                    {
                        res = 1.;
                        break;
                    }
                    float y = s.dist*s.dist/(2.0*ph)/12.;
                    float da = sqrt(s.dist*s.dist-y*y);
                    res = min( res, 100.0*da/max(0.0,d1-y) );
                    ph = s.dist;
                    d1 += min(s.dist,s.dist>5.e-1?1.e-2:5.e-3);
                }
                col = mix(.5*col, col, res);
            }
        }
    }

    s = s1;

    // Color drift
    if(s.material != 0.)
    {
        c1 = rgb2hsv(col);
        c1.r = pi*lfnoise(.1*nbeats*c.xx);
        col = mix(col, hsv2rgb(c1),.5);
        
        // Gamma
        col = col + col*col + col*col*col;
    }
    
    // Highlights
    col = mix(col, mix(col, col + col*col + col*col*col,.5), smoothstep(.9, 1.4, abs(dot(c.xzx, n))));

	fragColor = vec4(clamp(col,0.,1.),1.);

    // fragColor = mix(texture(iChannel0, fragCoord.xy/iResolution.xy), vec4(clamp(col,0.,1.),1.), .5);
}

void main()
{
	mainImage(out_color, gl_FragCoord.xy);
}