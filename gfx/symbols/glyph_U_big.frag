void rect(in vec2 uv, in vec4 rect, in vec2 shift, in float phi, in float scale, in float distort, inout float d);
void glyph_U_big(in vec2 uv, in vec2 shift, in float phi, in float scale, in float distort, inout float d){
rect(uv,vec4(3,2,1,4),shift,phi,scale,distort,d);rect(uv,vec4(3,2,2,1),shift,phi,scale,distort,d);rect(uv,vec4(10,2,1,12),shift,phi,scale,distort,d);rect(uv,vec4(2,3,1,13),shift,phi,scale,distort,d);rect(uv,vec4(9,4,1,13),shift,phi,scale,distort,d);rect(uv,vec4(1,6,2,8),shift,phi,scale,distort,d);rect(uv,vec4(8,9,1,9),shift,phi,scale,distort,d);rect(uv,vec4(7,13,3,2),shift,phi,scale,distort,d);rect(uv,vec4(7,13,5,1),shift,phi,scale,distort,d);rect(uv,vec4(2,14,2,2),shift,phi,scale,distort,d);rect(uv,vec4(3,14,1,3),shift,phi,scale,distort,d);rect(uv,vec4(5,14,1,3),shift,phi,scale,distort,d);rect(uv,vec4(5,14,2,2),shift,phi,scale,distort,d);rect(uv,vec4(2,15,5,1),shift,phi,scale,distort,d);rect(uv,vec4(3,15,3,2),shift,phi,scale,distort,d);
}