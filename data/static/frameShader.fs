void PLUG_main_texture_apply(
   inout vec4 fragment_color,
   const in vec3 normal)
{
  float avgcolor = (fragment_color[0] * 0.2126) + (fragment_color[1] * 0.7152) + (fragment_color[2] * 0.0722); // Grayscale
  fragment_color.xyz = vec3(avgcolor, (avgcolor * 0.5893), (avgcolor * 0.1785)); // Sepia Tint
}
