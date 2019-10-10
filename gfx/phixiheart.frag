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

void rot(in vec3 p, out mat3 rot)
{
    rot = mat3(c.xyyy, cos(p.x), sin(p.x), 0., -sin(p.x), cos(p.x))
        *mat3(cos(p.y), 0., -sin(p.y), c.yxy, sin(p.y), 0., cos(p.y))
        *mat3(cos(p.z), -sin(p.z), 0., sin(p.z), cos(p.z), c.yyyx);
}

// Hash function
void rand(in vec2 x, out float num)
{
    x += 400.;
    num = fract(sin(dot(sign(x)*abs(x) ,vec2(12.9898,78.233)))*43758.5453);
}

// Arbitrary-frequency 2D noise
void lfnoise(in vec2 t, out float num)
{
    vec2 i = floor(t);
    t = fract(t);
    //t = ((6.*t-15.)*t+10.)*t*t*t;  // TODO: add this for slower perlin noise
    t = smoothstep(c.yy, c.xx, t); // TODO: add this for faster value noise
    vec2 v1, v2;
    rand(i, v1.x);
    rand(i+c.xy, v1.y);
    rand(i+c.yx, v2.x);
    rand(i+c.xx, v2.y);
    v1 = c.zz+2.*mix(v1, v2, t.y);
    num = mix(v1.x, v1.y, t.x);
}

// Multi-frequency 2D noise
void mfnoise(in vec2 x, in float fmin, in float fmax, in float alpha, out float num)
{
    num = 0.;
    float a = 1., nf = 0., buf;
    for(float f = fmin; f<fmax; f = f*2.)
    {
        lfnoise(f*x, buf);
        num += a*buf;
        a *= alpha;
        nf += 1.;
    }
    num *= (1.-alpha)/(1.-pow(alpha, nf));
}

// Distance to line segment
void dlinesegment(in vec2 x, in vec2 p1, in vec2 p2, out float d)
{
    vec2 da = p2-p1;
    d = length(x-mix(p1, p2, clamp(dot(x-p1, da)/dot(da,da),0.,1.)));
}

// 2D box
void dbox(in vec2 x, in vec2 b, out float d)
{
    vec2 da = abs(x)-b;
    d = length(max(da,c.yy)) + min(max(da.x,da.y),0.0);
}

void dheart(in vec2 x, in float R, out float dst)
{
    x.x *= (1.-mix(0.,1.8*R, x.y));
    x.y *= (1.+.4*R*R);
    float p = atan(x.y,abs(x.x));
    dst = length(x);
    dst -= mix(R,-.5*R, p/2./pi+.5);
}

void stroke(in float d, in float h, out float dst)
{
    dst = abs(d)-h;
}

// Extrusion
void zextrude(in float z, in float d2d, in float h, out float d)
{
    vec2 w = vec2(-d2d, abs(z)-0.5*h);
    d = length(max(w,0.0));
}

void dphixiheart2(in vec2 x, in float w, out float d)
{
    float h = w/a, 
        wsingle = w/3.,
        da,
        dw = w/25.,
        scale = .9;
    
    // Phi
    // o
    d = length(x+wsingle*c.xy)-.5*(wsingle-2.*dw)*scale;
    stroke(d, dw, d);
    
    // horizontal
    dbox(vec2(x.x,abs(x.y))-vec2(-wsingle,.5*h-.5*dw), .5*vec2(scale*wsingle, 2.*dw), da);
    d = min(d, da);

    // vectical
    dbox(x-vec2(-wsingle,0.), .5*vec2(2.*dw, h), da);
    d = min(d, da);
    
    // decorations
    dbox(abs(x+wsingle*c.xy)-vec2(.5*scale*wsingle-dw, .5*h-1.*dw), vec2(dw, 1.5*dw), da);
    d = min(d, da);
    
    // make hole
    d = max(d, -length(x+wsingle*c.xy)+.5*(wsingle-4.*dw)*scale);
    
    // Xi
    // center
    dbox(x, .5*vec2(.6*scale*wsingle, 1.5*dw), da);
    d = min(d, da);

    // center decorations
    dbox(vec2(abs(x.x)-.25*scale*wsingle,x.y), vec2(dw, 1.5*dw), da);
    d = min(d, da);
    
    // horizontal
    dbox(vec2(x.x,abs(x.y))-vec2(0.,.5*h-.5*dw), .5*vec2(scale*wsingle, 2.*dw), da);
    d = min(d, da);
    
    // decorations
    dbox(abs(x)-vec2(.5*scale*wsingle-dw, .5*h-1.*dw), vec2(dw, 1.5*dw), da);
    d = min(d, da);
    
    // Heart
    // horizontal
    dbox(vec2(x.x,abs(x.y))-vec2(wsingle,.5*h-.5*dw), .5*vec2(scale*wsingle, 2.*dw), da);
    d = min(d, da);
    
    // decorations
    dbox(abs(x-wsingle*c.xy)-vec2(.5*scale*wsingle-dw, .5*h-1.*dw), vec2(dw, 1.5*dw), da);
    d = min(d, da);
    
    // actual heart
    dheart(x-vec2(wsingle,.125*h), .75*h, da);
    d = min(d, da);
}

void colorize(in vec2 uv, out vec3 col)
{
    float d, dborder, dbborder, noise, na;
    
    mfnoise(uv, 12.,1200.,.45, noise);
    dphixiheart2(uv, a, d);
    vec3 newc = mix(.6*c.yyx, c.yyx, .5+.5*noise);
    stroke(noise, .21, na);
    newc = mix(newc, .0*c.xyy, smoothstep(1.5/iResolution.y, -1.5/iResolution.y, na));
    col = mix(col, newc, smoothstep(1.5/iResolution.y, -1.5/iResolution.y, d));
    stroke(na, .1, na);
    newc = mix(newc, vec3(.1,.1,.4), smoothstep(1.5/iResolution.y, -1.5/iResolution.y, na));
    col = mix(col, newc, smoothstep(1.5/iResolution.y, -1.5/iResolution.y, d));
    
    stroke(d, .02, dborder);
    col = mix(col, .0*c.xxx, smoothstep(1.5/iResolution.y, -1.5/iResolution.y, dborder));
    
    stroke(dborder, .01, dbborder);
    col = mix(col, .8*c.yyx+.1*c.xyy+.3*c.yxy, smoothstep(1.5/iResolution.y, -1.5/iResolution.y, dbborder));
}

void scene(in vec3 x, out vec2 sdf)
{
    float d;

    // Texture
    float noise;
    mfnoise(x.xy, 12.,1200.,.45, noise);
    
    dphixiheart2(x.xy, a, d);
    float da;
    stroke(d, .02, da);
    zextrude(x.z, -d+.03, mix(.2+.01*noise,.3, step(da,0.)), sdf.x);
    sdf.y = 1.;
    
    // Add guard objects for debugging
    float dr = .05;
    vec3 y = mod(x,dr)-.5*dr;
    float guard = -length(max(abs(y)-vec3(.5*dr*c.xx, .6),0.));
    guard = abs(guard)+dr*.1;
    sdf.x = min(sdf.x, guard);
}

void normal(in vec3 x, out vec3 n)
{
    const float dx = 5.e-4;
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

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    a = iResolution.x/iResolution.y;
    vec2 uv = fragCoord/iResolution.yy-0.5*vec2(a, 1.0);
    vec3 col = c.yyy;
    
    uv *= mix(3.,2.,iScale);
    
    vec3 o = c.yyx, 
        t = vec3(uv, 0.),
        dir = normalize(t-o),
        n, x;
    vec2 s;
    float d = (-o.z+.2)/dir.z;
    int N = 450,
        i;
    
    for(i = 0; i<N; ++i)
    {
        x = o + d * dir;
        scene(x, s);
        if(s.x < 1.e-4) break;
        d += s.x;
        if(x.z < -.1) break;
    }
    
    if(i < N)
    {
        vec3 l = normalize(x - c.yyx),
            c1;
        colorize(x.xy, c1);
        c1 = mix(c1, c.yyy, 5.*abs(x.z));
        col = .6*c1
            + .5*c1*abs(dot(l,n))
            + 4.8*c1*abs(pow(dot(reflect(-l,n),dir),3.));
        
        mat3 m;
        rot(.3*vec3(1.1,1.2,1.3)*iTime, m);
        col = abs(m*col);
    }
    else col = c.yyy;
    
    
    //uv *= 2.;
    //colorize(uv, col);
    col *= col;
    
    col = clamp(col, 0., 1.);
    fragColor = vec4(col,1.0);
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy);
}
