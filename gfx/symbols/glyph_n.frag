void rect(in vec2 uv, in vec4 rect, in vec2 shift, in float phi, in float scale, in float distort, inout float d);
void glyph_n(in vec2 uv, in vec2 shift, in float phi, in float scale, in float distort, inout float d){
rect(uv,vec4(2,6,1,7),shift,phi,scale,distort,d);rect(uv,vec4(5,6,3,2),shift,phi,scale,distort,d);rect(uv,vec4(1,7,1,8),shift,phi,scale,distort,d);rect(uv,vec4(4,7,1,2),shift,phi,scale,distort,d);rect(uv,vec4(4,7,6,1),shift,phi,scale,distort,d);rect(uv,vec4(8,7,2,3),shift,phi,scale,distort,d);rect(uv,vec4(9,7,1,7),shift,phi,scale,distort,d);rect(uv,vec4(1,8,4,1),shift,phi,scale,distort,d);rect(uv,vec4(8,9,3,1),shift,phi,scale,distort,d);rect(uv,vec4(9,9,2,3),shift,phi,scale,distort,d);rect(uv,vec4(8,11,1,5),shift,phi,scale,distort,d);rect(uv,vec4(0,12,1,4),shift,phi,scale,distort,d);
}
