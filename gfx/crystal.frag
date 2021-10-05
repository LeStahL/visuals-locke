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

// TODO: hexagon pattern background or grid, or starfield
// TODO: maybe splines? does that look cool? hm ne
// TODO: another (smaller spikes) as well?
// TODO: add cyber net? 
// Param: inward/outward increase of crystal radius
// Param: Number of spikes
// Param: post laplace amount
// Param: rotation speed
// Sync: Crystal size and radius

const vec3 c = vec3(1.,0.,-1.),
    b = vec3(1.5,1.e3,1.5);
const float fsaa = 144.,
    f = 1.e3,
    ms = 2.,
    pi = 3.14159;
float ss = .02;
const float PHI = 1.618;
vec3 data[20] = vec3[20](
        vec3(0, PHI, 1), // Dodecahedron
        vec3(0, -PHI, 1),
        vec3(1, 0, PHI),
        vec3(-1, 0, PHI),
        vec3(PHI, 1, 0),
        vec3(-PHI, 1, 0), // Icosahedron, Octahedron
        vec3(1, 1, 1),
        vec3(-1, 1, 1),
        vec3(1, -1, 1),
        vec3(1, 1, -1),
        vec3(0, 1, PHI+1.),
        vec3(0, -1, PHI+1.),
        vec3(PHI+1., 0, 1),
        vec3(-PHI-1., 0, 1),
        vec3(1, PHI+1., 0),
        vec3(-1, PHI+1., 0),
        vec3(sqrt(2.), sqrt(6.), 1.), // Tetrahedron
        vec3(-sqrt(8.), 0., 1.),
        vec3(sqrt(2.), -sqrt(6.), 1.),
        vec3(0., 0., -3.)
    );
mat3 RR;

// Inspired here, modified for sizecoding and removed loads of
// unneccessary code: https://www.shadertoy.com/view/WdlGRf
// The paper they reference is very useful for understanding the regular polyhedron distances
// fHedron(0,6): Dodecahedron
// fHedron(6,10): Octahedron
// fHedron(6,16): Icosahedron
float fHedron(vec3 p, int offset, int len, float r, bool symmetric)
{
    float d = 0.,
        da;
    for(int i=offset; i<len; ++i)
    {
        da = dot(p, normalize(data[i]));
        d = max(d, symmetric?abs(da):da);
    }
    return d - r;
}

float dbox3(vec3 x, vec3 b)
{
  b = abs(x) - b;
  return length(max(b,0.))
         + min(max(b.x,max(b.y,b.z)),0.);
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
float hash13(vec3 p3)
{
	p3  = fract(p3 * .1031);
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

// Creative Commons Attribution-ShareAlike 4.0 International Public License
// Created by David Hoskins.
// See https://www.shadertoy.com/view/4djSRW
vec2 hash23(vec3 p3)
{
	p3 = fract(p3 * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+33.33);
    return fract((p3.xx+p3.yz)*p3.zy);
}

// Creative Commons Attribution-ShareAlike 4.0 International Public License
// Created by David Hoskins.
// See https://www.shadertoy.com/view/4djSRW
vec3 hash33(vec3 p3)
{
	p3 = fract(p3 * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yxz+33.33);
    return fract((p3.xxy + p3.yxx)*p3.zyx);
}

// Creative Commons Attribution-ShareAlike 4.0 International Public License
// Created by David Hoskins.
// See https://www.shadertoy.com/view/4djSRW
vec3 hash32(vec2 p)
{
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yxz+33.33);
    return fract((p3.xxy+p3.yzz)*p3.zyx);
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

// Multi-frequency fractal noise stack
float mfnoise(vec2 x, float d, float b, float e)
{
    float n = 0.;
    float a = 1., nf = 0., buf;
    for(float f = d; f<b; f *= 2.)
    {
        n += a*lfnoise(f*x);
        a *= e;
        nf += 1.;
    }
    return n * (1.-e)/(1.-pow(e, nf));
}

float tline(vec3 x, vec3 p1, vec3 p2)
{
    vec3 da = p2-p1;
    return clamp(dot(x-p1, da)/dot(da,da),0.,1.);
}

float dline(vec3 x, vec3 p1, vec3 p2)
{
    return length(x-mix(p1, p2, tline(x,p1,p2)));
}

// Analytical box distance.
// Use this by plugging o-x0 into x.
vec2 abox3(vec3 x, vec3 dir, vec3 s)
{
    vec3 a = (s-x)/dir, 
        b = -(s+x)/dir,
        dn = min(a,b),
        df = max(a,b);
    return vec2(
        all(lessThan(abs(x + dn.y * dir).zx,s.zx)) 
            ? dn.y 
            : all(lessThan(abs(x + dn.x * dir).yz,s.yz)) 
                ? dn.x 
                : all(lessThan(abs(x + dn.z * dir).xy,s.xy)) 
                    ? dn.z
                    : f,
        all(lessThan(abs(x + df.y * dir).zx,s.zx)) 
            ? df.y 
            : all(lessThan(abs(x + df.x * dir).yz,s.yz)) 
                ? df.x 
                : all(lessThan(abs(x + df.z * dir).xy,s.xy)) 
                    ? df.z 
                    : f
    );
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

vec3 hsv2rgb(vec3 cc)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(cc.xxx + K.xyz) * 6.0 - K.www);
    return cc.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), cc.y);
}

// Compute an orthonormal system from a single vector in R^3
mat3 ortho(vec3 d)
{
    vec3 a = normalize(
        d.x != 0. 
            ? vec3(-d.y/d.x,1.,0.)
            : d.y != 0.
                ? vec3(1.,-d.x/d.y,0.)
                : vec3(1.,0.,-d.x/d.z)
    );
    return mat3(d, a, cross(d,a));
}

// Rotation in R3
mat3 rot3(in vec3 p)
{
    return mat3(c.xyyy, cos(p.x), sin(p.x), 0., -sin(p.x), cos(p.x))
        *mat3(cos(p.y), 0., -sin(p.y), c.yxy, sin(p.y), 0., cos(p.y))
        *mat3(cos(p.z), -sin(p.z), 0., sin(p.z), cos(p.z), c.yyyx);
}

// Inverse spherical fibonacci mapping tech by las/mercury
// Originally from https://www.shadertoy.com/view/lllXz4
// Modified by fizzer to put out the vector q.
// Modified by NR4 to reduce size.
vec2 inverseSF( vec3 p, float n, out vec3 outq ) 
{
    float m = 1. - 1./n,
        phi = min(atan(p.y, p.x), pi), cosTheta = p.z,
        k  = max(2., floor( log(n * pi * sqrt(5.) * (1.0 - cosTheta*cosTheta))/ log(PHI+1.))),
        Fk = pow(PHI, k)/sqrt(5.0),
        d,j;
    vec2  F  = vec2( round(Fk), round(Fk * PHI) ),
        ka = 2.*F/n,
        kb = 2.*pi*( fract((F+1.0)*PHI) - (PHI-1.) ),
        c;    
    mat2 iB = mat2( ka.y, -ka.x, kb.y, -kb.x ) / (ka.y*kb.x - ka.x*kb.y);
    
    c = floor( iB * vec2(phi, cosTheta - m));
    d = 8.;
    j = 0.;
    for( int s=0; s<4; s++ ) 
    {
        vec2 uv = vec2( float(s-2*(s/2)), float(s/2) );
        
        float i = round(dot(F, uv + c)),
            phi = 2.0*pi*fract(i*PHI),
            cosTheta = m - 2.0*i/n,
            sinTheta = sqrt(1.0 - cosTheta*cosTheta);
        vec3 q = vec3( cos(phi)*sinTheta, sin(phi)*sinTheta, cosTheta );
        float squaredDistance = dot(q-p, q-p);
        
        if (squaredDistance < d) 
        {
            outq = q;
            d = squaredDistance;
            j = i;
        }
    }
    return vec2( j, sqrt(d) );
}

// Analytical sphere distance.
// Use this by plugging o-x0 into x.
vec2 asphere(vec3 x, vec3 dir, float R)
{
    float a = dot(dir,dir),
        b = 2.*dot(x,dir),
        cc = dot(x,x)-R*R,
        dis = b*b-4.*a*cc;
    if(dis<0.) return vec2(f);
    vec2 dd = (c.xz*sqrt(dis)-b)/2./a;
    return vec2(min(dd.x, dd.y), max(dd.x, dd.y));
}

vec3 add(vec3 a, vec3 b)
{
    if(a.y < b.y) return a;
    return b;
}

vec3 z;
vec3 scene(vec3 x)
{
    x = RR * x;
    inverseSF(normalize(x), 62.+42.*lfnoise(iTime*c.xx), z);
    //mat3 n = ortho(normalize(z));
    //vec2 ra = 2.*hash23(z)-1.;

    vec3 p1 = c.yyy,
        p2 = (.4+.05*lfnoise(z.xy+z.yz+z.xz-iTime))*z;// + .02*n[1]*ra.x + .02*n[2]*ra.y;
    mat3 m = ortho(normalize(p2-p1)),
        mt = transpose(m);
    float t = tline(x, p1, p2);
    vec3 pn = mix(p1,p2,t),
        y = mt * (x - pn);
    
    return vec3(2., abs(fHedron(y, 6, 16, mix(.02,.06,t), true))-.002, .5);
}

vec2 as;
bool ray(inout vec3 col, out vec3 x, float d, vec3 dir, out vec3 s, vec3 o, vec3 l, out vec3 n)
{
    for(int i=0; i<550; ++i)
    {
        x = o + d * dir;
        s = scene(x);
        
        if(s.y > as.y) break;
        
        if(s.y < 5.e-5)
        {
            // Blinn-Phong Illumination
            float dx = 1.e-5;
            n = normalize(vec3(
                scene(x+dx*c.xyy).y, 
                scene(x+dx*c.yxy).y, 
                scene(x+dx*c.yyx).y
            )-s.y);

            //if(s.x == 0.)
            //    col = .1*c.xxx;
                vec3 colors[8] = vec3[8](
                    vec3(0.02,0.00,0.17),
                    vec3(.20,.23,.27),
                    vec3(0.32,0.38,0.98),
                    vec3(1.00,0.18,0.33),
                    vec3(.83,.15,.17),
                    vec3(.94,.65,.07).zyx,
                    vec3(.85, 1, .20),
                    vec3(.23, .46, .24)
                );
                
                s.x += hash13(z-.13);
                
                float i = mod(floor(s.x), 8.),
                    ip1 = mod(i + 1., 8.);
                col = mix(colors[int(i)], colors[int(ip1)], fract(s.x));

                col += .1*hash33(z);

                col = .2 * col
                    + .4*col * max(dot(normalize(l-x),n),0.)
                    + .7 * col*pow(max(dot(reflect(normalize(l-x),n),dir),0.),2.);
                    
                col = mix(col, 2.3*col, 2.*clamp(.5-length(x),0.,1.));
                    
                return true;
        }
        d += s.y<2.e-1?min(s.y,5.e-3):s.y;//min(s.y,1.e-3);
    }
    return false;
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    RR = rot3(.3*vec3(1.1,1.3,1.5)*iTime);
    vec2 uv = fragCoord/iResolution.xy,
        unit = 1./iResolution.xy;
    ivec2 index = ivec2(fragCoord);
    
    vec2 uv1 = (fragCoord-.5*iResolution.xy)/iResolution.y;
    vec3 
        o = c.yzx,
        t = c.yyy,
        col,
        c1,
        x,
        x1,
        n,
        j = .0001*(2.*hash32(1.e2*uv1-iTime)-1.),
        dir = normalize(t + uv1.x * c.xyy + uv1.y * cross(c.xyy,normalize(t-o))-o)+j,
        l = c.yxx,
        s,
        s1;
    
    as = asphere(o,dir, .55);
    if(as.x < 1.e1)
    {
        // Material ray
        if(ray(col, x, as.x, dir, s, o, l, n))
        {

            // Refractions
            for(int i=0; i<5; ++i)
            {
                dir = refract(dir,n, .9);
                if(ray(c1, x1, 2.e-2, dir+ .0005*(2.*hash32(1.e2*uv1-iTime+float(i))-1.), s1, x, l, n))
                {
                    col = mix(col, c1, s.z);
                    s = s1;
                }
            }
            // Hard Shadow
            //if(ray(c1, x1, 1.e-2, normalize(l-x), s1, x, l, n) && length(l-x1) < length(l-x))
         //       col *= .5;
        }
    }
    
    //col = mix(col, vec3(0.85,0.00,0.69), .3);

    //col *= 1.3;

    // Gamma
    col += col*col + col*col*col;
    //col *= .75;
    //fragColor = vec4(clamp(col,0.,1.),1.);
    
    //col = mix(col, 2.*col, clamp(.5-length(x),0.,1.));
    
	fragColor = vec4(clamp(col,0.,1.),1.);
    // fragColor = mix(texture(iChannel0, uv), vec4(clamp(col,0.,1.),1.), .5);
}

void main()
{
	mainImage(gl_FragColor, gl_FragCoord.xy);
}