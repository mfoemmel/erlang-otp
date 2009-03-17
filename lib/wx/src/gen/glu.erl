%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%

%% OPENGL UTILITY API

%% This file is generated DO NOT EDIT

%% @doc  A part of the standard OpenGL Utility api. 
%% See <a href="http://www.opengl.org/sdk/docs/man/">www.opengl.org</a>
%%
%% Booleans are represented by integers 0 and 1. 

%% @type enum().   An integer defined in gl.hrl
%% @type offset(). An integer which is an offset in an array
%% @type clamp().  A float clamped between 0.0 - 1.0 

-module(glu).
-compile(inline).
-define(GLenum,32/native-unsigned).
-define(GLboolean,8/native-unsigned).
-define(GLbitfield,32/native-unsigned).
-define(GLbyte,8/native-signed).
-define(GLshort,16/native-signed).
-define(GLint,32/native-signed).
-define(GLubyte,8/native-unsigned).
-define(GLushort,16/native-unsigned).
-define(GLuint,32/native-unsigned).
-define(GLsizei,32/native-signed).
-define(GLfloat,32/native-float).
-define(GLclampf,32/native-float).
-define(GLdouble,64/native-float).
-define(GLclampd,64/native-float).
-define(GLsizeiptr,64/native-unsigned).
-define(GLintptr,64/native-unsigned).
-define(GLhandleARB,32/native-unsigned).

-export([tesselate/2,build1DMipmapLevels/9,build1DMipmaps/6,build2DMipmapLevels/10,
  build2DMipmaps/7,build3DMipmapLevels/11,build3DMipmaps/8,checkExtension/2,
  errorString/1,getString/1,lookAt/9,ortho2D/4,perspective/4,pickMatrix/5,
  project/6,unProject/6,unProject4/9]).


%% API 

%% @spec (Vec3, [Vec3]) -> {Triangles, VertexPos}
%%  Vec3 = {float(),float(),float()}
%%  Triangles = [VertexIndex::integer()]
%%  VertexPos  = binary()
%% @doc General purpose polygon triangulation.
%% The first argument is the normal and the second a list of
%% vertex positions. Returned is a list of indecies of the vertices
%% and a binary (64bit native float) containing an array of 
%% vertex positions, it starts with the vertices in Vs and 
%% may contain newly created vertices in the end.
tesselate({Nx,Ny,Nz}, Vs) ->
  wxe_util:call(5000, <<(length(Vs)):32/native,0:32,
    Nx:?GLdouble,Ny:?GLdouble,Nz:?GLdouble,
    (<< <<Vx:?GLdouble,Vy:?GLdouble,Vz:?GLdouble >>
        || {Vx,Vy,Vz} <- Vs>>)/binary >>).

%% @spec (Target::enum(),InternalFormat::integer(),Width::integer(),Format::enum(),Type::enum(),Level::integer(),Base::integer(),Max::integer(),Data::binary()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild1DMipmapLevels.xml">external</a> documentation.
build1DMipmapLevels(Target,InternalFormat,Width,Format,Type,Level,Base,Max,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:call(5001, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum,Level:?GLint,Base:?GLint,Max:?GLint>>).

%% @spec (Target::enum(),InternalFormat::integer(),Width::integer(),Format::enum(),Type::enum(),Data::binary()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild1DMipmaps.xml">external</a> documentation.
build1DMipmaps(Target,InternalFormat,Width,Format,Type,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:call(5002, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),InternalFormat::integer(),Width::integer(),Height::integer(),Format::enum(),Type::enum(),Level::integer(),Base::integer(),Max::integer(),Data::binary()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild2DMipmapLevels.xml">external</a> documentation.
build2DMipmapLevels(Target,InternalFormat,Width,Height,Format,Type,Level,Base,Max,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:call(5003, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum,Level:?GLint,Base:?GLint,Max:?GLint>>).

%% @spec (Target::enum(),InternalFormat::integer(),Width::integer(),Height::integer(),Format::enum(),Type::enum(),Data::binary()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild2DMipmaps.xml">external</a> documentation.
build2DMipmaps(Target,InternalFormat,Width,Height,Format,Type,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:call(5004, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (Target::enum(),InternalFormat::integer(),Width::integer(),Height::integer(),Depth::integer(),Format::enum(),Type::enum(),Level::integer(),Base::integer(),Max::integer(),Data::binary()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild3DMipmapLevels.xml">external</a> documentation.
build3DMipmapLevels(Target,InternalFormat,Width,Height,Depth,Format,Type,Level,Base,Max,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:call(5005, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,Type:?GLenum,Level:?GLint,Base:?GLint,Max:?GLint>>).

%% @spec (Target::enum(),InternalFormat::integer(),Width::integer(),Height::integer(),Depth::integer(),Format::enum(),Type::enum(),Data::binary()) -> integer()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluBuild3DMipmaps.xml">external</a> documentation.
build3DMipmaps(Target,InternalFormat,Width,Height,Depth,Format,Type,Data) ->
  wxe_util:send_bin(Data),
  wxe_util:call(5006, <<Target:?GLenum,InternalFormat:?GLint,Width:?GLsizei,Height:?GLsizei,Depth:?GLsizei,Format:?GLenum,Type:?GLenum>>).

%% @spec (ExtName::[integer()],ExtString::[integer()]) -> 0|1
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluCheckExtension.xml">external</a> documentation.
checkExtension(ExtName,ExtString) ->
  wxe_util:call(5007, <<(length(ExtName)):?GLuint,
        (<< <<C:?GLubyte>> || C <- ExtName>>)/binary,0:((8-((length(ExtName)+ 4) rem 8)) rem 8),(length(ExtString)):?GLuint,
        (<< <<C:?GLubyte>> || C <- ExtString>>)/binary,0:((8-((length(ExtString)+ 4) rem 8)) rem 8)>>).

%% @spec (Error::enum()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluErrorString.xml">external</a> documentation.
errorString(Error) ->
  wxe_util:call(5008, <<Error:?GLenum>>).

%% @spec (Name::enum()) -> string()
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluGetString.xml">external</a> documentation.
getString(Name) ->
  wxe_util:call(5009, <<Name:?GLenum>>).

%% @spec (EyeX::float(),EyeY::float(),EyeZ::float(),CenterX::float(),CenterY::float(),CenterZ::float(),UpX::float(),UpY::float(),UpZ::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluLookAt.xml">external</a> documentation.
lookAt(EyeX,EyeY,EyeZ,CenterX,CenterY,CenterZ,UpX,UpY,UpZ) ->
  wxe_util:cast(5010, <<EyeX:?GLdouble,EyeY:?GLdouble,EyeZ:?GLdouble,CenterX:?GLdouble,CenterY:?GLdouble,CenterZ:?GLdouble,UpX:?GLdouble,UpY:?GLdouble,UpZ:?GLdouble>>).

%% @spec (Left::float(),Right::float(),Bottom::float(),Top::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluOrtho2D.xml">external</a> documentation.
ortho2D(Left,Right,Bottom,Top) ->
  wxe_util:cast(5011, <<Left:?GLdouble,Right:?GLdouble,Bottom:?GLdouble,Top:?GLdouble>>).

%% @spec (Fovy::float(),Aspect::float(),ZNear::float(),ZFar::float()) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluPerspective.xml">external</a> documentation.
perspective(Fovy,Aspect,ZNear,ZFar) ->
  wxe_util:cast(5012, <<Fovy:?GLdouble,Aspect:?GLdouble,ZNear:?GLdouble,ZFar:?GLdouble>>).

%% @spec (X::float(),Y::float(),DelX::float(),DelY::float(),Viewport::{integer()}) -> ok
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluPickMatrix.xml">external</a> documentation.
pickMatrix(X,Y,DelX,DelY,{V1,V2,V3,V4}) ->
  wxe_util:cast(5013, <<X:?GLdouble,Y:?GLdouble,DelX:?GLdouble,DelY:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @spec (ObjX::float(),ObjY::float(),ObjZ::float(),Model::{float()},Proj::{float()},View::{integer()}) -> {integer(),WinX::float(),WinY::float(),WinZ::float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluProject.xml">external</a> documentation.
project(ObjX,ObjY,ObjZ,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16},{V1,V2,V3,V4}) ->
  wxe_util:call(5014, <<ObjX:?GLdouble,ObjY:?GLdouble,ObjZ:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,P13:?GLdouble,P14:?GLdouble,P15:?GLdouble,P16:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @spec (WinX::float(),WinY::float(),WinZ::float(),Model::{float()},Proj::{float()},View::{integer()}) -> {integer(),ObjX::float(),ObjY::float(),ObjZ::float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluUnProject.xml">external</a> documentation.
unProject(WinX,WinY,WinZ,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16},{V1,V2,V3,V4}) ->
  wxe_util:call(5015, <<WinX:?GLdouble,WinY:?GLdouble,WinZ:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,P13:?GLdouble,P14:?GLdouble,P15:?GLdouble,P16:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint>>).

%% @spec (WinX::float(),WinY::float(),WinZ::float(),ClipW::float(),Model::{float()},Proj::{float()},View::{integer()},NearVal::float(),FarVal::float()) -> {integer(),ObjX::float(),ObjY::float(),ObjZ::float(),ObjW::float()}
%% @doc See <a href="http://www.opengl.org/sdk/docs/man/xhtml/gluUnProject.xml">external</a> documentation.
unProject4(WinX,WinY,WinZ,ClipW,{M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16},{P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16},{V1,V2,V3,V4},NearVal,FarVal) ->
  wxe_util:call(5016, <<WinX:?GLdouble,WinY:?GLdouble,WinZ:?GLdouble,ClipW:?GLdouble,M1:?GLdouble,M2:?GLdouble,M3:?GLdouble,M4:?GLdouble,M5:?GLdouble,M6:?GLdouble,M7:?GLdouble,M8:?GLdouble,M9:?GLdouble,M10:?GLdouble,M11:?GLdouble,M12:?GLdouble,M13:?GLdouble,M14:?GLdouble,M15:?GLdouble,M16:?GLdouble,P1:?GLdouble,P2:?GLdouble,P3:?GLdouble,P4:?GLdouble,P5:?GLdouble,P6:?GLdouble,P7:?GLdouble,P8:?GLdouble,P9:?GLdouble,P10:?GLdouble,P11:?GLdouble,P12:?GLdouble,P13:?GLdouble,P14:?GLdouble,P15:?GLdouble,P16:?GLdouble,V1:?GLint,V2:?GLint,V3:?GLint,V4:?GLint,NearVal:?GLdouble,FarVal:?GLdouble>>).

