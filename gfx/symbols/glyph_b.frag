void rect(in vec2 uv, in vec4 rect, in vec2 shift, in float phi, in float scale, in float distort, inout float d);
void glyph_b(in vec2 uv, in vec2 shift, in float phi, in float scale, in float distort, inout float d){
rect(uv,vec4(2,3,2,2),shift,phi,scale,distort,d);rect(uv,vec4(3,3,1,3),shift,phi,scale,distort,d);rect(uv,vec4(2,4,3,1),shift,phi,scale,distort,d);rect(uv,vec4(3,4,2,2),shift,phi,scale,distort,d);rect(uv,vec4(4,4,1,13),shift,phi,scale,distort,d);rect(uv,vec4(3,5,3,1),shift,phi,scale,distort,d);rect(uv,vec4(4,5,2,4),shift,phi,scale,distort,d);rect(uv,vec4(7,9,2,2),shift,phi,scale,distort,d);rect(uv,vec4(4,10,2,4),shift,phi,scale,distort,d);rect(uv,vec4(4,10,5,1),shift,phi,scale,distort,d);rect(uv,vec4(9,11,1,2),shift,phi,scale,distort,d);rect(uv,vec4(3,12,2,5),shift,phi,scale,distort,d);rect(uv,vec4(9,12,2,1),shift,phi,scale,distort,d);rect(uv,vec4(10,12,1,4),shift,phi,scale,distort,d);rect(uv,vec4(2,14,3,3),shift,phi,scale,distort,d);rect(uv,vec4(2,15,4,2),shift,phi,scale,distort,d);rect(uv,vec4(9,15,1,2),shift,phi,scale,distort,d);rect(uv,vec4(1,16,9,1),shift,phi,scale,distort,d);
}