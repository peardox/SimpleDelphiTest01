uniform sampler2D mask_texture;

void PLUG_texture_color(inout vec4 texture_color, const in sampler2D alpha_texture, const in vec4 tex_coord) {
  float alpha = texture2D(mask_texture, tex_coord.st).r; // r=g=b so pick one...
  texture_color = vec4(texture_color.xyz, alpha);
}
