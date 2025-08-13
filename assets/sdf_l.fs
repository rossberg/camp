#version 330

// Input vertex attributes
in vec2 fragTexCoord;
in vec4 fragColor;

// Input uniform values
uniform sampler2D texture0;
uniform vec4 colDiffuse;

// Output fragment color
out vec4 finalColor;

void main()
{
  float distOutline = texture(texture0, fragTexCoord).a - 0.47;
  float distChange = length(vec2(dFdx(distOutline), dFdy(distOutline)));
  float alpha = smoothstep(-distChange, distChange, distOutline);
  finalColor = vec4(fragColor.rgb, fragColor.a * alpha);
}
