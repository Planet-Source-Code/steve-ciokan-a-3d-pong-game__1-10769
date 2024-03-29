xof 0302txt 0064
template Header {
 <3D82AB43-62DA-11cf-AB39-0020AF71E433>
 WORD major;
 WORD minor;
 DWORD flags;
}

template Vector {
 <3D82AB5E-62DA-11cf-AB39-0020AF71E433>
 FLOAT x;
 FLOAT y;
 FLOAT z;
}

template Coords2d {
 <F6F23F44-7686-11cf-8F52-0040333594A3>
 FLOAT u;
 FLOAT v;
}

template Matrix4x4 {
 <F6F23F45-7686-11cf-8F52-0040333594A3>
 array FLOAT matrix[16];
}
template ColorRGBA {
 <35FF44E0-6C7C-11cf-8F52-0040333594A3>
 FLOAT red;
 FLOAT green;
 FLOAT blue;
 FLOAT alpha;
}

template ColorRGB {
 <D3E16E81-7835-11cf-8F52-0040333594A3>
 FLOAT red;
 FLOAT green;
 FLOAT blue;
}

template IndexedColor {
 <1630B820-7842-11cf-8F52-0040333594A3>
 DWORD index;
 ColorRGBA indexColor;
}

template Boolean {
 <4885AE61-78E8-11cf-8F52-0040333594A3>
 WORD truefalse;
}

template Boolean2d {
 <4885AE63-78E8-11cf-8F52-0040333594A3>
 Boolean u;
 Boolean v;
}

template MaterialWrap {
 <4885AE60-78E8-11cf-8F52-0040333594A3>
 Boolean u;
 Boolean v;
}

template TextureFilename {
 <A42790E1-7810-11cf-8F52-0040333594A3>
 STRING filename;
}

template Material {
 <3D82AB4D-62DA-11cf-AB39-0020AF71E433>
 ColorRGBA faceColor;
 FLOAT power;
 ColorRGB specularColor;
 ColorRGB emissiveColor;
 [...]
}

template MeshFace {
 <3D82AB5F-62DA-11cf-AB39-0020AF71E433>
 DWORD nFaceVertexIndices;
 array DWORD faceVertexIndices[nFaceVertexIndices];
}

template MeshFaceWraps {
 <4885AE62-78E8-11cf-8F52-0040333594A3>
 DWORD nFaceWrapValues;
 Boolean2d faceWrapValues;
}

template MeshTextureCoords {
 <F6F23F40-7686-11cf-8F52-0040333594A3>
 DWORD nTextureCoords;
 array Coords2d textureCoords[nTextureCoords];
}

template MeshMaterialList {
 <F6F23F42-7686-11cf-8F52-0040333594A3>
 DWORD nMaterials;
 DWORD nFaceIndexes;
 array DWORD faceIndexes[nFaceIndexes];
 [Material]
}

template MeshNormals {
 <F6F23F43-7686-11cf-8F52-0040333594A3>
 DWORD nNormals;
 array Vector normals[nNormals];
 DWORD nFaceNormals;
 array MeshFace faceNormals[nFaceNormals];
}

template MeshVertexColors {
 <1630B821-7842-11cf-8F52-0040333594A3>
 DWORD nVertexColors;
 array IndexedColor vertexColors[nVertexColors];
}

template Mesh {
 <3D82AB44-62DA-11cf-AB39-0020AF71E433>
 DWORD nVertices;
 array Vector vertices[nVertices];
 DWORD nFaces;
 array MeshFace faces[nFaces];
 [...]
}

template FrameTransformMatrix {
 <F6F23F41-7686-11cf-8F52-0040333594A3>
 Matrix4x4 frameMatrix;
}

template Frame {
 <3D82AB46-62DA-11cf-AB39-0020AF71E433>
 [...]
}
Header {
 1;
 0;
 1;
}

Material mat_default {
	0.800000;0.800000;0.800000;1.000000;;
	32.000000;
	0.700000;0.700000;0.700000;;
	0.000000;0.000000;0.000000;;
}

Mesh Cube0 {
 8;
 -13.228348;-14.186634;-52.352829;,
 -13.228348;11.325176;-52.352829;,
 12.283463;11.325176;-52.352829;,
 12.283463;-14.186634;-52.352829;,
 12.283463;11.325176;-26.841019;,
 12.283463;-14.186634;-26.841019;,
 -13.228348;11.325176;-26.841019;,
 -13.228348;-14.186634;-26.841019;;

 24;
 3;0,1,2;,
 3;2,2,1;,
 3;2,3,0;,
 3;0,0,3;,
 3;3,2,4;,
 3;4,4,2;,
 3;4,5,3;,
 3;3,3,5;,
 3;5,4,6;,
 3;6,6,4;,
 3;6,7,5;,
 3;5,5,7;,
 3;7,6,1;,
 3;1,1,6;,
 3;1,0,7;,
 3;7,7,0;,
 3;2,1,6;,
 3;6,6,1;,
 3;6,4,2;,
 3;2,2,4;,
 3;0,3,5;,
 3;5,5,3;,
 3;5,7,0;,
 3;0,0,7;;

 MeshMaterialList {
  1;
  24;
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0;;
  {mat_default}
 } # MeshMaterialList

 MeshNormals {
 72;
 0.000000;0.000000;1.000000;,
 0.000000;0.000000;1.000000;,
 0.000000;0.000000;1.000000;,
 -0.333333;-0.666667;0.666667;,
 -0.333333;-0.666667;0.666667;,
 0.816497;-0.408248;0.408248;,
 0.000000;0.000000;1.000000;,
 0.000000;0.000000;1.000000;,
 0.000000;0.000000;1.000000;,
 0.333333;0.666667;0.666667;,
 0.333333;0.666667;0.666667;,
 -0.816497;0.408248;0.408248;,
 -1.000000;0.000000;0.000000;,
 -1.000000;0.000000;0.000000;,
 -1.000000;0.000000;0.000000;,
 -0.816497;-0.408248;-0.408248;,
 -0.816497;-0.408248;-0.408248;,
 -0.333333;-0.666667;0.666667;,
 -1.000000;0.000000;0.000000;,
 -1.000000;0.000000;0.000000;,
 -1.000000;0.000000;0.000000;,
 -0.816497;0.408248;0.408248;,
 -0.816497;0.408248;0.408248;,
 -0.333333;0.666667;-0.666667;,
 0.000000;0.000000;-1.000000;,
 0.000000;0.000000;-1.000000;,
 0.000000;0.000000;-1.000000;,
 0.333333;-0.666667;-0.666667;,
 0.333333;-0.666667;-0.666667;,
 -0.816497;-0.408248;-0.408248;,
 0.000000;0.000000;-1.000000;,
 0.000000;0.000000;-1.000000;,
 0.000000;0.000000;-1.000000;,
 -0.333333;0.666667;-0.666667;,
 -0.333333;0.666667;-0.666667;,
 0.816497;0.408248;-0.408248;,
 1.000000;0.000000;0.000000;,
 1.000000;0.000000;0.000000;,
 1.000000;0.000000;0.000000;,
 0.816497;-0.408248;0.408248;,
 0.816497;-0.408248;0.408248;,
 0.333333;-0.666667;-0.666667;,
 1.000000;0.000000;0.000000;,
 1.000000;0.000000;0.000000;,
 1.000000;0.000000;0.000000;,
 0.816497;0.408248;-0.408248;,
 0.816497;0.408248;-0.408248;,
 0.333333;0.666667;0.666667;,
 0.000000;-1.000000;0.000000;,
 0.000000;-1.000000;0.000000;,
 0.000000;-1.000000;0.000000;,
 0.333333;-0.666667;-0.666667;,
 0.333333;-0.666667;-0.666667;,
 0.816497;-0.408248;0.408248;,
 0.000000;-1.000000;0.000000;,
 0.000000;-1.000000;0.000000;,
 0.000000;-1.000000;0.000000;,
 -0.333333;-0.666667;0.666667;,
 -0.333333;-0.666667;0.666667;,
 -0.816497;-0.408248;-0.408248;,
 0.000000;1.000000;0.000000;,
 0.000000;1.000000;0.000000;,
 0.000000;1.000000;0.000000;,
 -0.333333;0.666667;-0.666667;,
 -0.333333;0.666667;-0.666667;,
 -0.816497;0.408248;0.408248;,
 0.000000;1.000000;0.000000;,
 0.000000;1.000000;0.000000;,
 0.000000;1.000000;0.000000;,
 0.333333;0.666667;0.666667;,
 0.333333;0.666667;0.666667;,
 0.816497;0.408248;-0.408248;;

 24;
 3;0,1,2;,
 3;3,4,5;,
 3;6,7,8;,
 3;9,10,11;,
 3;12,13,14;,
 3;15,16,17;,
 3;18,19,20;,
 3;21,22,23;,
 3;24,25,26;,
 3;27,28,29;,
 3;30,31,32;,
 3;33,34,35;,
 3;36,37,38;,
 3;39,40,41;,
 3;42,43,44;,
 3;45,46,47;,
 3;48,49,50;,
 3;51,52,53;,
 3;54,55,56;,
 3;57,58,59;,
 3;60,61,62;,
 3;63,64,65;,
 3;66,67,68;,
 3;69,70,71;;
 } # MeshNormals
} # Mesh

