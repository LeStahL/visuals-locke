/* Hardcyber - PC-64k-Intro by Team210 at Deadline 2k19
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
 
#version 130

// void scale(out float s);

uniform float iTime;
uniform vec2 iResolution;
uniform vec2 iScale;

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

uniform sampler2D iChannel0;
uniform float iFSAA;

out vec4 gl_FragColor;

const float pi = 3.14159;
const vec3 c = vec3(1.,0.,-1.);
float a = 1.0;

float lscale, rscale;
float size;

float nbeats;
// float iScale;

void hsv2rgb(in vec3 hsv, out vec3 rgb)
{
    float C = hsv.y * hsv.z,
        Hprime = hsv.x / pi * 3.,
        X = C * (1.-abs(mod(Hprime,2.)-1.));
    
    if(0. <= Hprime && Hprime <= 1.) rgb = vec3(C, X, 0.);
    else if( 1. < Hprime && Hprime <= 2.) rgb = vec3(X, C, 0.);
    else if( 2. < Hprime && Hprime <= 3.) rgb = vec3(0., C, X);
    else if( 3. < Hprime && Hprime <= 4.) rgb = vec3(0., X, C);
    else if( 4. < Hprime && Hprime <= 5.) rgb = vec3(X, 0., C);
    else if( 5. < Hprime && Hprime <= 6.) rgb = vec3(C, 0., X);
        
    float m = hsv.z - C;
    rgb += m;
}

void rgb2hsv(in vec3 rgb, out vec3 hsv)
{
    float MAX = max(rgb.r, max(rgb.g, rgb.b)),
        MIN = min(rgb.r, min(rgb.g, rgb.b)),
        C = MAX-MIN;
    
    if(MAX == MIN) hsv.x = 0.;
    else if(MAX == rgb.r) hsv.x = pi/3.*(rgb.g-rgb.b)/C;
    else if(MAX == rgb.g) hsv.x = pi/3.*(2.+(rgb.b-rgb.r)/C);
    else if(MAX == rgb.b) hsv.x = pi/3.*(4.+(rgb.r-rgb.g)/C);
    hsv.x = mod(hsv.x, 2.*pi);
        
    if(MAX == 0.) hsv.y = 0.;
    else hsv.y = (MAX-MIN)/MAX;
        
    hsv.z = MAX;
}

void rand(in vec2 x, out float n);
void lfnoise(in vec2 t, out float n);
void stroke(in float d0, in float s, out float d);
void dvoronoi(in vec2 x, out float d, out vec2 p, out float control_distance);
void hash22(in vec2 x, out vec2 y);
void rot3(in vec3 p, out mat3 rot);
float sm(float d)
{
    return smoothstep(1.5/iResolution.y, -1.5/iResolution.y, d);
}

float dot2( in vec3 v ) { return dot(v,v); }

void palette(in float scale, out vec3 col)
{
    const int N = 5;
    vec3 colors[N] = vec3[N](
             vec3(0.72,0.88,0.95),
             vec3(0.09,0.60,0.66),
             vec3(0.67,0.83,0.34),
             vec3(0.98,0.79,0.03),
             vec3(0.95,0.35,0.27)
    );
	float index = floor(scale*float(N)), 
        remainder = scale*float(N)-index;
    col = mix(colors[int(index)],colors[int(index)+1], remainder);
}

// Adapted from https://www.shadertoy.com/view/4sXXRN
void dtriangle3(in vec3 p,  in vec3 v1, in vec3 v2, in vec3 v3, out float dst)
{
    vec3 v21 = v2 - v1; vec3 p1 = p - v1;
    vec3 v32 = v3 - v2; vec3 p2 = p - v2;
    vec3 v13 = v1 - v3; vec3 p3 = p - v3;
    vec3 nor = cross( v21, v13 );

    dst = sqrt( (sign(dot(cross(v21,nor),p1)) + 
                  sign(dot(cross(v32,nor),p2)) + 
                  sign(dot(cross(v13,nor),p3))<2.0) 
                  ?
                  min( min( 
                  dot2(v21*clamp(dot(v21,p1)/dot2(v21),0.0,1.0)-p1), 
                  dot2(v32*clamp(dot(v32,p2)/dot2(v32),0.0,1.0)-p2) ), 
                  dot2(v13*clamp(dot(v13,p3)/dot2(v13),0.0,1.0)-p3) )
                  :
                  dot(nor,p1)*dot(nor,p1)/dot2(nor) );
}

void rot3(in vec3 p, out mat3 rot);
void dbox3(in vec3 x, in vec3 b, out float d);
void add(in vec2 sda, in vec2 sdb, out vec2 sdf);
mat3 R;
void scene(in vec3 x, out vec2 sdf)
{
    float d;
    
	// Big red box    
    dbox3(x, .2*c.xxx, sdf.x);
    sdf.y = 1.;
    
    // Holes
    
    // 2 upper bar
    dbox3(x-.1*c.xyy, vec3(.02,.3,.12), d);
    sdf.x = max(-d, sdf.x);
    sdf.y = mix(sdf.y, 2., step(d, sdf.x));
    
    // 2 right bar
    dbox3(x-.05*c.xyy-.1*c.yyx, vec3(.07,.3,.02), d);
    sdf.x = max(-d, sdf.x);
    sdf.y = mix(sdf.y, 2., step(d, sdf.x));
    
    // 2 mid bar
    dbox3(x, vec3(.02,.3,.1), d);
    sdf.x = max(-d, sdf.x);
    sdf.y = mix(sdf.y, 2., step(d, sdf.x));
    
    // 2 left bar
    dbox3(x+.05*c.xyy+.1*c.yyx, vec3(.07,.3,.02), d);
    sdf.x = max(-d, sdf.x);
    sdf.y = mix(sdf.y, 2., step(d, sdf.x));
    
    // 2 dot
    dbox3(x+.1*c.xyy-.1*c.yyx, vec3(.02,.3,.02), d);
    sdf.x = max(-d, sdf.x);
    sdf.y = mix(sdf.y, 2., step(d, sdf.x));
    
    // 1 bar
    dbox3(x+.04*c.yyx, vec3(.3,.02,.08), d);
    sdf.x = max(-d, sdf.x);
    sdf.y = mix(sdf.y, 2., step(d, sdf.x));
    
    // 1 dot
    dbox3(x-.1*c.yyx, vec3(.3,.02,.02), d);
    sdf.x = max(-d, sdf.x);
    sdf.y = mix(sdf.y, 2., step(d, sdf.x));
    
    // 0 big stripes
    vec3 y = vec3(x.x, abs(x.y), x.z);
    dbox3(y-.05*c.yxy, vec3(.1,.03,.3), d);
    sdf.x = max(-d, sdf.x);
    sdf.y = mix(sdf.y, 2., step(d, sdf.x));

	// 0 small stripes
    dbox3(y-.1*c.yxy-.06*c.xyy, vec3(.08,.021,.3), d);
    sdf.x = max(-d, sdf.x);
    sdf.y = mix(sdf.y, 2., step(d, sdf.x));

    // 0 upper/lower stripes
    vec3 z = vec3(abs(x.x), x.yz);
	dbox3(z-.119*c.xyy, vec3(.021,.08,.3), d);
    sdf.x = max(-d, sdf.x);
    sdf.y = mix(sdf.y, 2., step(d, sdf.x));
}

void scene2(in vec3 x, out vec2 sdf)
{
    float v = 0., vn;
    vec2 vi = c.yy;
    dvoronoi(x.xy/size, v, vi, vn);
    vec3 y = vec3(x.xy-vi*size, x.z);
    vec2 yi = vi*size;
    
    float n = 0.;
    lfnoise(4.*(yi-.5*iTime), n);
    lfnoise(12.*vec2(n,1.)*yi-(.8+.2*n)*c.xy, n);
    n *= iScale;
    //sdf = vec2(length(y-.05*n*c.yyx)-.5*size, 1.);
//     sdf = vec2(length(y-.05*n*c.yyx)-mix(.05,1.,length(texture(iChannel0, yi/vec2(a,1.)).rgb)/sqrt(3.))*size, 1.);
    sdf = vec2(length(y-.05*n*c.yyx)-size*vn*mix(1.,4.,length(texture(iChannel0, yi/vec2(a,1.)).rgb)/sqrt(3.)), 1.);
}

void normal2(in vec3 x, out vec3 n, in float dx)
{
    vec2 s, na;
    
    scene2(x,s);
    scene2(x+dx*c.xyy, na);
    n.x = na.x;
    scene2(x+dx*c.yxy, na);
    n.y = na.x;
    scene2(x+dx*c.yyx, na);
    n.z = na.x;
    n = normalize(n-s.x);
}

void scene3(in vec3 x, out vec2 sdf)
{
    vec3 y = vec3(mod(x.xy,2.*size)-size, x.z);
    vec2 yi = x.xy-y.xy;
    float ss = mix(.0,.05,size/.01);
    
    vec2 p0 = .8*size*c.xx,
        p1 = .8*size*c.zx,
        p2 = .8*size*c.xz;
    
    vec2 ind;
    
    float y0, y1, y2;
    lfnoise(4.e1*(yi+p0-.5e-4*iTime), y0);
    lfnoise(12.e1*vec2(y0,1.)*(yi+p0)-1.e-4*(.8+.2*y0)*iTime*c.xy, y0);
    lfnoise(4.e1*(yi+p1-.5e-4*iTime), y1);
    lfnoise(12.e1*vec2(y1,1.)*(yi+p1)-1.e-4*(.8+.2*y1)*iTime*c.xy, y1);
    lfnoise(4.e1*(yi+p2-.5e-4*iTime), y2);
    lfnoise(12.e1*vec2(y2,1.)*(yi+p2)-1.e-4*(.8+.2*y2)*iTime*c.xy, y2);
    y0 *= ss;
    y1 *= ss;
    y2 *= ss;
    
    dtriangle3(y, vec3(p0,y0), vec3(p1,y1), vec3(p2,y2), sdf.x);
    
    float d;
    vec2 p3 = .8*size*c.zz,
        p4 = .8*size*c.xz,
        p5 = .8*size*c.zx;
    
    float y3, y4, y5;
    lfnoise(4.e1*(yi+p3-.5e-4*iTime), y3);
    lfnoise(12.e1*vec2(y3,1.)*(yi+p3)-1.e-4*(.8+.2*y3)*iTime*c.xy, y3);
    lfnoise(4.e1*(yi+p4-.5e-4*iTime), y4);
    lfnoise(12.e1*vec2(y4,1.)*(yi+p4)-1.e-4*(.8+.2*y4)*iTime*c.xy, y4);
    lfnoise(4.e1*(yi+p5-.5e-4*iTime), y5);
    lfnoise(12.e1*vec2(y5,1.)*(yi+p5)-1.e-4*(.8+.2*y5)*iTime*c.xy, y5);
    y3 *= ss;
    y4 *= ss;
    y5 *= ss;
    
    dtriangle3(y, vec3(p3,y3), vec3(p4,y4), vec3(p5,y5), d);
    sdf.x = min(sdf.x, d);

    stroke(sdf.x, .1*size, sdf.x);
    sdf.y = 1.;
}

void normal3(in vec3 x, out vec3 n, in float dx)
{
    vec2 s, na;
    
    scene3(x,s);
    scene3(x+dx*c.xyy, na);
    n.x = na.x;
    scene3(x+dx*c.yxy, na);
    n.y = na.x;
    scene3(x+dx*c.yyx, na);
    n.z = na.x;
    n = normalize(n-s.x);
}

void normal(in vec3 x, out vec3 n, in float dx);
void mainImage( out vec4 fragColor, in vec2 fragCoord_ )
{
    vec2 fragCoord = fragCoord_;
    vec4 col = vec4(0.);
    float bound = sqrt(iFSAA)-1.;
    
    float delta = 0.;
    vec2 n;
    
    // Chromatic distortion
    if(iFader0 > 0.) 
    {
        delta = mix(.0,.02,iFader0);
        rand(floor(20.*fragCoord.y/iResolution.y*c.xx-1337.*floor(12.*iTime)),n.x);
        rand(floor(20.*fragCoord.y/iResolution.y*c.xx-1337.*floor(12.*iTime)+2337.),n.y);
    }
    
    // HF noise
    if(iFader1 > 0.)
    {
        lfnoise(12.*fragCoord-iTime, n.x);
        lfnoise(12.*fragCoord-iTime-1337., n.y);
        fragCoord += mix(1.,20.,iFader1)*n;
    }
    
    // LF noise
    if(iFader2 > 0.)
    {
        lfnoise(22.*fragCoord/iResolution-3.*iTime, n.x);
        lfnoise(22.*fragCoord/iResolution-3.*iTime-1337., n.y);
        fragCoord += mix(0.,22.,iFader2)*n;
    }
    
    // Kaleidoscope
    if(iFader3 > 0. || iFader4 > 0.)
    {
        float a = iResolution.x/iResolution.y;
        // vec2 uv = fragCoord/iResolution.yy-0.5*vec2(a, 1.0);
        vec2 uv = (fragCoord.xy -.5*iResolution.xy-iDial7)/iResolution.y;
//         rand(floor(.33*iTime)*c.xx, n.x);
//         n.x = max(floor(12.*n.x),3.);
        n.x = round(mix(3.,10.,iFader3));
        n.y = round(mix(3.,10.,iFader4));

        vec2 rs = 1.*vec2(1.,pi)/(.1+n.xy), 
            rp = vec2(length(uv), mod(9.*pi+atan(uv.y, uv.x), 2.*pi)),
            drp = abs(mod(rp, rs)-.5*rs)+pi*c.yx,
            rpj = rp-drp;
        uv = drp.x*vec2(cos(drp.y), sin(drp.y));

        // if(round(rpj/rs).x == 2.) col.rgb = c.yxy;
        // float phi = abs(mod(atan(uv.y, uv.x),pi/n.x)-.5*pi/n.x);
        // uv = length(uv)*vec2(cos(phi), sin(phi));
        
        fragCoord = (uv*iResolution.y + .5*iResolution.xy);
    }
    
    // // Voronoi tiles
    // Boxes/Spheres post! Not fader 4
    // if(iFader4 > 0.)
    // {
    //     float a = iResolution.x/iResolution.y;
    //     vec2 uv = fragCoord/iResolution.yy-0.5*vec2(a, 1.0);
        
    //     float dv, vp;
    //     vec2 ind;
    //     dvoronoi(mix(1.,100.,1.-iFader4)*uv, dv, ind, vp);
    //     uv = ind/mix(1.,100.,1.-iFader4);
        
    //     fragCoord = (uv + .5*vec2(a,1.))*iResolution.yy;
    // }
    
   	for(float i = -.5*bound; i<=.5*bound; i+=1.)
        for(float j=-.5*bound; j<=.5*bound; j+=1.)
        {
            vec3 cl = texture(iChannel0, fragCoord/iResolution.xy+delta*n+vec2(i,j)*3.0/max(bound,1.)/iResolution.xy).rgb,
                cr = texture(iChannel0, fragCoord/iResolution.xy-delta*n+vec2(i,j)*3.0/max(bound,1.)/iResolution.xy).rgb,
                cc = texture(iChannel0, fragCoord/iResolution.xy+vec2(i,j)*3.0/max(bound,1.)/iResolution.xy).rgb;
            col += vec4(cl.r, cc.g, cr.b,1.);
        }
    col /= iFSAA;
    
    // Color
    if(iFader5 > 0.)
    {
//         mat3 RR;
//         vec3 ra;
//         rand(iFader5*c.xx+3337., ra.x);
//         rand(iFader5*c.xx+1337., ra.y);
//         rand(iFader5*c.xx+2337., ra.z);
//         rot3((iFader5-48.)*810.*ra+col.rgb,RR);
//         col.rgb = mix(col.rgb, abs(RR*col.rgb),col.rgb);
        vec3 c1;
        palette(iFader5, c1);
        col.rgb = mix(col.rgb, c1, length(col.rgb)/sqrt(3.)); 
        
    }
    
    // Grayscale
    if(iFader6 > 0.)
    {
        col.rgb = mix(col.rgb, length(col.rgb)/sqrt(3.)*c.xxx, iFader6);
    }

    // Fade to black
    if(iDial0 > 0.)
    {
        col.rgb = mix(col.rgb, c.yyy, iDial0);
    }

    // Glow
    if(iDial1 > 0.)
    {
        float a = iResolution.x/iResolution.y;
        vec2 uv = fragCoord/iResolution.xy;
        vec2 unit =  1./iResolution.xy;
        vec4 col11 = texture(iChannel0, uv - unit),
            col13 = texture(iChannel0, uv + unit*c.xz),
            col31 = texture(iChannel0, uv + unit*c.zx),
            col33 = texture(iChannel0, uv + unit),
            x = col33 -col11 -3.* texture(iChannel0, uv + unit*c.yz) -col13 + col31 + 3.*texture(iChannel0, uv + unit*c.yx),
            y = col33 -col11 -3.* texture(iChannel0, uv + unit*c.zy) -col31 + col13 + 3.*texture(iChannel0, uv + unit*c.xy);
        col = vec4(mix(col.rgb, .5*(abs(y.rgb) + abs(x.rgb)).rgb, iDial1), 1.);
    }

    // Gamma
    if(iDial2 > 0.)
    {
        col.rgb = mix(col, col + col*col + col * col * col, iDial2).rgb;
    }

    // Strobe
    if(iDial3 > 0.)
    {
        float onOff = 1./(1.+10.*iDial3),
            dt = mod(iTime, onOff)-.5*onOff;
        if(dt > 0.)
            col.rgb = mix(col.rgb, c.xxx, .7);
    }

    // hue modulation
    if(iDial4 > 0.)
    {
        vec3 hsv;
        rgb2hsv(col.rgb, hsv);
        hsv.r = mod(hsv.r + 10.*(-1.+2.*iDial4), 2.*pi);
        hsv2rgb(hsv, col.rgb);
    }

    // saturation decrease
    if(iDial5 > 0.)
    {
        vec3 hsv;
        rgb2hsv(col.rgb, hsv);
        hsv.g = mod(hsv.g*(1.-iDial5), 2.*pi);
        hsv2rgb(hsv, col.rgb);
    }
    
    // saturation increase
    if(iDial6 > 0.)
    {
        vec3 hsv;
        rgb2hsv(col.rgb, hsv);
        hsv.g = mod(hsv.g*(1.+iDial6), 2.*pi);
        hsv2rgb(hsv, col.rgb);
    }



    
//     vec3 as = texture(iChannel0, fragCoord/iResolution).rgb;
//     vec2 nb;
//     lfnoise((as.xy+as.yz+as.xz), nb.x);
//     lfnoise((as.xy+as.yz+as.xz), nb.y);
//     fragCoord += 22.*(.1*as.r + .2*as.g + .3*as.b);
    
    float a = iResolution.x/iResolution.y;
    vec2 uv = fragCoord/iResolution.yy-0.5*vec2(a, 1.0);
    
    // scale(iScale);
    
// //     nbeats = mod(iTime, 60./29.);
//     iScale = nbeats-30./29.;
//     iScale = smoothstep(-5./29., 0., iScale)*(1.-smoothstep(0., 15./29., iScale));
    
//     vec3 col = texture(iChannel0, fragCoord/iResolution).rgb;
//     float delta = 0.;
//     vec2 n = c.yy;
    /*
    // Box
    rot3(vec3(-2.*pi/8.,2.*pi/8.,2.*pi/4.)-iTime*vec3(1.1,1.3,1.5), R);
    
    float d;
    vec2 s;
    vec3 o, r, u, t, ssize, dir, x, n;
    vec2 uv2 = 10.*(uv-vec2(-.45*a,.45));
    o = R * c.yyx;
	r = c.xyy; 
	u = c.yxy;
	t = c.yyy; 
    int N = 250,
        i;
    t = uv2.x * r + uv2.y * u;
    t = R * t;
    dir = normalize(t-o);

    ssize = .2*c.xxx;

	vec3 tlo = min((ssize-o)/dir,(-ssize-o)/dir); // Select 3 visible planes
    vec2 abxlo = abs(o.yz + tlo.x*dir.yz),
        abylo = abs(o.xz + tlo.y*dir.xz),
        abzlo = abs(o.xy + tlo.z*dir.xy);

    vec4 dn = 100.*c.xyyy;

    dn = mix(dn, vec4(tlo.x,c.xyy), float(all(lessThan(abxlo,ssize.yz)))*step(tlo.x,dn.x));
    dn = mix(dn, vec4(tlo.y,c.yxy), float(all(lessThan(abylo,ssize.xz)))*step(tlo.y,dn.x));
    dn = mix(dn, vec4(tlo.z,c.yyx), float(all(lessThan(abzlo,ssize.xy)))*step(tlo.z,dn.x));
    
    uv = (fragCoord)/iResolution.xy*vec2(a,1.);
    
    d = dn.r;
    
    float nan;
    lfnoise(iTime*c.xx, nan);
    nan += .5;
    if(nan > 0.) d = 3.;
    
    if(d<=2.)
    {
        x = o + d * dir;
        scene(x,s);
        
        if(s.x > 1.e-4)
        {
            for(i = 0; i<N; ++i)
            {
                x = o + d * dir;
                scene(x,s);
                if(s.x < 1.e-4)break;
                d += s.x;
            }
        }
        
        if(i<N)
        {
            normal(x,n, 5.e-4);
            
            if(s.y == 1.)
            {
                vec3 l = normalize(x+c.zzx*vec3(1.3,.9,1.2));
                col = vec3(0.81,0.15,0.18);
                col = .3*col
                    + .4*col * abs(dot(l,n))
                    + .6 * col * abs(pow(dot(reflect(-l,n),dir),2.));
            }
            else if(s.y == 2.)
            {
                vec3 l = normalize(x+c.zzx*vec3(1.3,.9,1.2));
                col = .7*c.xxx;
                col = .5*col
                    + .4*col * abs(dot(l,n))
                    + .8 * col * abs(pow(dot(reflect(-l,n),dir),2.));
            }
        }
        
        if(iTime < 0.) col = texture(iChannel0, fragCoord/iResolution).rgb;
    }
    else
    {
//         iScale = nbeats-30./29.;
//         iScale = smoothstep(-5./29., 0., iScale)*(1.-smoothstep(0./29., 35./29., iScale));
//         lscale = iScale;
//         lscale = smoothstep(61.233,62.233,iTime)*(1.-smoothstep(71.385,72.385,iTime));
//         lscale += smoothstep(0.,.5,clamp((iTime-10.),0.,1.))*(1.-smoothstep(0.,.5,clamp((iTime-18.),0.,1.)));
//         rscale = smoothstep(167.,167.5,iTime)-smoothstep(172.,172.5,iTime);
        
//         rscale = iScale;
//         rscale = 0.;
//         lscale = 0.;
//         size = mix(.005, .01, rscale);
//         size = mix(0., size, max(rscale, lscale));
     
        // voronoi balls
        if(iFader7 > 0.)
        {
            lscale = iFader7;
            col = c.yyy;
            
            o = c.yyx+.5*vec3(cos(iTime), sin(iTime),0.);
            r = c.xyy;
            u = c.yxy;
            t = c.yyy;
            dir = c.yyy;
            n = c.yyy;
            x = c.yyy;
            N = 200;
            t = uv.x * r + uv.y * u;
            dir = normalize(t-o);

            d = -(o.z-.05-.5*size)/dir.z;
            
            for(i = 0; i<N; ++i)
            {
                x = o + d * dir;
                scene2(x,s);
                if(s.x < 1.e-4)break;
                
                if(x.z<-.05-.5*size)
                {
                    col = c.yyy;
                    i = N;
                    break;
                }
                d += min(s.x,1.e-3);
                //d += s.x;
            }
            
            if(i < N)
            {
                normal2(x,n, 5.e-4);
                vec3 l = normalize(x+.5*n);
            
                if(s.y == 1.)
                {
                    float v, vn;
                    vec2 vi;
                    dvoronoi(x.xy/size, v, vi, vn);
                    vec3 y = vec3(x.xy-vi*size, x.z);
                    vec2 yi = vi*size;
                    
                    float bound = sqrt(iFSAA)-1.;

                    for(float i = -.5*bound; i<=.5*bound; i+=1.)
                        for(float j=-.5*bound; j<=.5*bound; j+=1.)
                        {
                            col += texture(iChannel0, yi/vec2(a,1.)+vec2(i,j)*3./max(bound, 1.)/iResolution.xy).xyz;
                        }
                    col /= iFSAA;   
                    
                    col = .4*col
                        + .9*col * abs(dot(l,n))
                        + .6*col * pow(abs(dot(reflect(-l,n),dir)),3.);
                }
            }
            else col = c.yyy;
        }
        else if(rscale > 0.)
        {
            col = c.yyy;
            
            o = c.yyx+.5*vec3(-1., -1.,0.);
            r = c.xyy;
            u = c.yxy;
            t = c.yyy;
            dir = c.yyy;
            n = c.yyy;
            x = c.yyy;
            N = 300;
            t = uv.x * r + uv.y * u;
            dir = normalize(t-o);

            d = -(o.z-.05-.5*size)/dir.z;
            
            for(i = 0; i<N; ++i)
            {
                x = o + d * dir;
                scene3(x,s);
                if(s.x < 1.e-4)break;
                
                if(x.z<-.05-.5*size)
                {
                    col = c.yyy;
                    i = N;
                    break;
                }
                d += min(s.x,1.e-3);
                //d += s.x;
            }
            
            if(i < N)
            {
                normal3(x,n, 5.e-4);
                vec3 l = normalize(x+.5*n);
            
                if(s.y == 1.)
                {
                    vec3 y = vec3(mod(x.xy,size)-.5*size, x.z);
                    vec2 yi = x.xy-y.xy;
                    
                    col = texture(iChannel0, yi/vec2(a,1.)).rgb;
                    
//                     col = .7*c.xxy;
                    
                    col = .4*col
                        + .9*col * abs(dot(l,n))
                        + .6*col * pow(abs(dot(reflect(-l,n),dir)),3.);
                    
                }
            }
            else col = c.yyy;
        }
    }
    
    
    
    fragColor = vec4(clamp(col,0.,1.),1.0);*/
    
    
    // Scan lines
    col += vec4(0., 0.05, 0.1,0.)*sin(uv.y*1050.+ 5.*iTime);
    fragColor = col;
//     fragColor = vec4(clamp(col,0.,1.),1.);
}

void main()
{
    mainImage(gl_FragColor, gl_FragCoord.xy);
}
