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

uniform float iNBeats;

const vec4 c = vec4(1.,0.,-1.,3.14159);
const float pi = 3.14159,
    PHI = 1.618,
    f = 1.e4;
mat3 R;
    
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
/*
// 3D noise function (IQ)
float noise(vec3 p)
{
	vec3 ip=floor(p),
        s=vec3(7, 157, 113);
	p-=ip;
	vec4 h=vec4(0, s.yz, s.y+s.z)+dot(ip, s);
	p=p*p*(3.-2.*p);
	h=mix(fract(sin(h)*43758.5), fract(sin(h+s.x)*43758.5), p.x);
	h.xy=mix(h.xz, h.yw, p.y);
	return mix(h.x, h.y, p.z);
}

// hash without sine: https://www.shadertoy.com/view/4djSRW
float hash11(float p)
{
    p = fract(p * .1031);
    p *= p + 19.19;
    p *= p + p;
    return fract(p);
}

float hash12(vec2 p)
{
	vec3 p3  = fract(vec3(p.xyx) * .1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

// method by fizzer
vec3 hashHs(vec3 n, float seed)
{
    float u = hash11( 78.233 + seed),
        v = hash11( 10.873 + seed),
        a = 6.2831853 * v;
    u = 2.0*u - 1.0;
    return normalize( n + vec3(sqrt(1.0-u*u) * vec2(cos(a), sin(a)), u) );
}*/

void hash33(in vec3 p3, out vec3 d)
{
	p3 = fract(p3 * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yxz+33.33);
    d = fract((p3.xxy + p3.yxx)*p3.zyx);
}

// Scene marching information
struct SceneData
{
    // Material for palette
    float material,
    
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
    return SceneData(.8, d, .0, .0, .0, .5, 1.);
}

SceneData add(SceneData a, SceneData b)
{
    if(a.dist < b.dist) return a;
    return b;
}

float dbox3(vec3 x, vec3 b)
{
  vec3 da = abs(x) - b;
  return length(max(da,0.))
         + min(max(da.x,max(da.y,da.z)),0.);
}

float m(vec2 x)
{
    return max(x.x,x.y);
}

float d210(vec2 x)
{
    return min(max(max(max(max(min(max(max(m(abs(vec2(abs(abs(x.x)-.25)-.25, x.y))-vec2(.2)), -m(abs(vec2(x.x+.5, abs(abs(x.y)-.05)-.05))-vec2(.12,.02))), -m(abs(vec2(abs(x.x+.5)-.1, x.y-.05*sign(x.x+.5)))-vec2(.02,.07))), m(abs(vec2(x.x+.5,x.y+.1))-vec2(.08,.04))), -m(abs(vec2(x.x, x.y-.04))-vec2(.02, .08))), -m(abs(vec2(x.x, x.y+.1))-vec2(.02))), -m(abs(vec2(x.x-.5, x.y))-vec2(.08,.12))), -m(abs(vec2(x.x-.5, x.y-.05))-vec2(.12, .07))), m(abs(vec2(x.x-.5, x.y))-vec2(.02, .08)));
}

float datz(vec2 uv)
{
    vec2 a = abs(uv)-.25;
    return max(max(min(max(min(abs(mod(uv.x-1./12.,1./6.)-1./12.)-1./30., abs(a.x+a.y)-.015),a.x+a.y), max(a.x+.1,a.y+.1)), -length(uv-vec2(0.,.04))+.045), -max(a.x+.225,a.y+.175));
}

void pR(inout vec2 p, float a)
{
	p = cos(a)*p+sin(a)*vec2(p.y, -p.x);
}

// iq's smooth minimum
float smoothmin(float a, float b, float k)
{
    float h = max( k-abs(a-b), 0.0 )/k;
    return min( a, b ) - h*h*h*k*(1./6.);
}

float smoothmax(float a, float b, float k)
{
    return a + b - smoothmin(a,b,k);
}

float zextrude(float z, float d2d, float h)
{
    vec2 w = vec2(d2d, abs(z)-.5*h);
    return min(max(w.x,w.y),0.) + length(max(w,0.));
}

SceneData scene(vec3 x)
{
    float s = .2,
        m = 0.;
    vec3 y = mod(x, s)-.5*s,
        yi = x-y,
        r,
        q;
    vec2 z = mod(x.xy, 1.5*s)-.75*s,
        w = mod(x.xy, .25*s)-.125*s;
    
    inverseSF(normalize(R*(x-.5*R*c.yxy)), 31.+ 120.*iScale, q);
    
    // Random material
    hash33(yi, r);
    r = 2.*r-1.;
    m += step(r.x+r.y+r.z,-1.);
    
    float rad = .2+.2*iScale;

    SceneData d = add(
        // Nippelball
        SceneData(0.,smoothmin(length(x-.5*R*c.yxy)-rad, length(R*(x-R*.5*c.yxy)-rad*q)-.04, .03), .3, .8, 0., .5, 1.),
        // Ceiling
        SceneData(3.,-x.z+.9, 0., .0, 0., 1., 1.5)
    );
    
    // Die einen Karos an der Decke
    d = add(d, SceneData(0.,dbox3(vec3(z,x.z-.9), vec3(.015,2.,.015)), 0., 0., 0., .3, .5));
    d = add(d, SceneData(0.,dbox3(vec3(z,x.z-.9), vec3(2.,.015,.015)), 0., 0., 0., .3, .5));
        
    // Floor
    // d = add(d, SceneData(2.,smoothmax(x.z+1.,-dbox3(vec3(w,x.z+1.), vec3(.07,.07,.05)*s),.1), 0., .1, 0., .5, 1.));

    // Kewl octree subdivision
    if(dbox3(yi, vec3(1.))>0.)
    {
        for(int i=0; i<4; ++i)
            if(r.y+r.z+r.x > 0.)
            {
                s *= .5; // Try .25
                vec3 a = mod(y, s)-.5*s,
                    yi = y-a;
                y = a;

                hash33(r +yi+.124*float(i), r);
                r = 2.*r-1.;
                m += step(r.x+r.y+r.z,-1.);
            }

        // Wall
        d = add(d, SceneData(m, dbox3(y, .39*s*c.xxx)-.1*s, 0., .1, 0., .3, .5+.5*m));
        }
    return d;
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
        vec3(0.21,0.21,0.21),
        vec3(0.94,0.39,0.23),
        vec3(0.94,0.39,0.23),
        c.xxx
    );
    float i = floor(scale),
        ip1 = mod(i + 1., float(N));
    return mix(colors[int(i)], colors[int(ip1)], fract(scale));
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

// Orthogonal projection for any vector d. Without the use of Gram-Schmidt for shorter code.
// This assumes, that you don't plug in the origin (which would be stupid, right?)
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


// Analytical infinite cylinder distance.
// Use this by plugging o-x0 into x.
vec2 apipe(vec3 x, vec3 dir, vec3 d, float R)
{
    //vec3 al = normalize(cross(d,dir));
    //mat3 m = transpose(mat3(al, cross(d, al), d));
    mat3 m = transpose(ortho(dir));
    x = m*x;
    dir = m*dir;
    
    return asphere(vec3(x.xy,0.), vec3(dir.xy,0.), R);
}

bool ray(out vec3 col, out vec3 x, inout float d, vec3 dir, out SceneData s, vec3 o, vec3 l, out vec3 n)
{
    for(int i=0; i<150; ++i)
    {
        x = o + d * dir;
        
        // Bounding box
        if(dbox3(x, 1.1*c.xxx) > 0.)
        {
            d = f;
            x = o + d * dir;
            return false;
        }

        
        s = scene(x);
        
        if(s.dist < 1.e-4) 
        {
            // Blinn-Phong Illumination
            n = normal(x);
            col = palette(s.material);
            col = .2 * col
                + s.diffuse * col*max(dot(normalize(l-x),n),0.)
                + s.specular * col*pow(max(dot(reflect(normalize(l-x),n),dir),0.),2.);
                
            return true;
        }
        
        float k = dbox3(x, .85*c.xxx);
        d += min(s.dist,k<0.?1.e-1:2.e-3);
        //d += s.dist;
        //d += min(s.dist, 1.e-3);
        
        //if(d > asphere(o-.5*R*c.yxy, dir, .24).y) d = abox3(o, dir, vec3(1.,1.,.911)).y;
    }
    return false;
}

mat3 rot3(vec3 p)
{
    return mat3(c.xyyy, cos(p.x), sin(p.x), 0., -sin(p.x), cos(p.x))
        *mat3(cos(p.y), 0., -sin(p.y), c.yxy, sin(p.y), 0., cos(p.y))
        *mat3(cos(p.z), -sin(p.z), 0., sin(p.z), cos(p.z), c.yyyx);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    R = rot3(mix(.1,1., iFader7)*vec3(1.1,1.3,1.7)*iTime);

    float d = 0.,
        d1;
    vec2 uv = (fragCoord.xy-.5*iResolution.xy)/iResolution.y;
    vec3 o = R*vec3(0., -.5, .5),
        col = c.yyy,
        c1,
        x,
        x1,
        n,
        n1,
        r = R*c.xyy,
        t = c.yyy,
        dir = normalize(uv.x * r + uv.y * cross(r,normalize(t-o))-o),
        l = c.yyy;
    SceneData s, 
        s1;
        
    // Material ray
    d = min(asphere(o-.5*R*c.yxy, dir, (.2+.2*iScale)+.04).x, abox3(o+.075*c.yyx, dir, vec3(1.,1.,.925)).y);
    
    if(ray(col, x, d, dir, s, o, l, n))
    {
        vec3 re = reflect(dir,n);
        // Reflections
        if(dbox3(x,vec3(1.)) < 0.) d1 = abox3(x+.075*c.yyx, re, vec3(1.,1.,.925)).y;
        else d1 = min(abox3(x+.075*c.yyx, re, vec3(1.,1.,.925)).y, asphere(x-.5*R*c.yxy, re, .24).x);
        if(ray(c1, x1, d1, re, s1, x, l, n1))
            col = mix(col, c1, s.reflectivity);
        /*
        d1 = asphere(o-.5*c.yxy, refract(dir,n, .99), .24).y;
        // Refractions
        d1 = 2.e-3;
        if(ray(c1, x1, d1, refract(dir,n, .99), s1, x, l, n1))
            col = mix(col, c1, s.transmittivity);
        */
        
        /*
        // Hard Shadow
        d1 = 1.e-2;
        if(ray(c1, x1, d1, normalize(l-x), s1, x, l, n1))
        {
            if(length(l-x1) < length(l-x))
                col *= .5;
        }
        */
    }
    /*
    // Laz0r
    float rl = .05;
    vec2 lazor = apipe(o-.5*R*c.yxy, dir, normalize(R*R*R*c.zzx), rl);
    if( lazor.x < f)
    {
        col = c.yxy;
        //col = mix(col, vec3(.9,.1,.09), smoothstep(1.5*rl,2.2*rl,abs(lazor.y-lazor.x)));
    }*/
    
    // Gamma
    //col = .6*(col+col*col);
    
    
    fragColor = vec4(clamp(col,0.,1.),1.);
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy);
}
