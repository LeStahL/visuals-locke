void rect(in vec2 uv, in vec4 rect, in vec2 shift, in float phi, in float scale, in float distort, inout float d);
void glyph_a(in vec2 uv, in vec2 shift, in float phi, in float scale, in float distort, inout float d){
rect(uv,vec4(5,6,1,3),shift,phi,scale,distort,d);rect(uv,vec4(5,6,2,2),shift,phi,scale,distort,d);rect(uv,vec4(8,6,1,10),shift,phi,scale,distort,d);rect(uv,vec4(4,7,1,3),shift,phi,scale,distort,d);rect(uv,vec4(4,7,5,1),shift,phi,scale,distort,d);rect(uv,vec4(3,8,1,7),shift,phi,scale,distort,d);rect(uv,vec4(8,8,2,3),shift,phi,scale,distort,d);rect(uv,vec4(2,10,2,5),shift,phi,scale,distort,d);rect(uv,vec4(7,10,2,4),shift,phi,scale,distort,d);rect(uv,vec4(1,11,3,3),shift,phi,scale,distort,d);rect(uv,vec4(1,13,4,1),shift,phi,scale,distort,d);rect(uv,vec4(2,13,3,2),shift,phi,scale,distort,d);rect(uv,vec4(4,13,1,3),shift,phi,scale,distort,d);rect(uv,vec4(6,13,1,2),shift,phi,scale,distort,d);rect(uv,vec4(6,13,4,1),shift,phi,scale,distort,d);rect(uv,vec4(8,13,2,3),shift,phi,scale,distort,d);rect(uv,vec4(2,14,5,1),shift,phi,scale,distort,d);rect(uv,vec4(4,14,2,2),shift,phi,scale,distort,d);
}