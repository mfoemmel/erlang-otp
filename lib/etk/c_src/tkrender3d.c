/*
**
** Tk interface to RenderWare
**
*/

#include <rwlib.h>
#include <rwx11dev.h>
#include <math.h>

/* #define DEBUG */

#include <tk.h>
#include <string.h>

#include "xcolors.h"


typedef struct
{
    RwReal x;
    RwReal y;
} RwV2d;


typedef struct Camera {
    Tk_Window tkwin;
    Display* display;
    Tcl_Interp* interp;
    Tcl_Command widgetCmd;

    XColor* backdrop_color;
    RwRaster* backdrop_image;
    char* backdrop_name;
    int   backdrop_offsetx;
    int   backdrop_offsety;
    int   backdrop_x;
    int   backdrop_y;
    int   backdrop_width;
    int   backdrop_height;

    int   max_height;
    int   max_width;

    int width;
    int height;

    Tk_Uid state;		/* Normal or disabled.  Entry is read-only
				  * when disabled. */

    Tk_3DBorder border;		/* Structure used to draw 3-D border and
				  * background.  NULL means no background
				  * or border. */
    int borderWidth;		/* Width of 3-D border (if any). */
    int relief;			/* 3-d effect: TK_RELIEF_RAISED etc. */
    int highlightWidth;

    /* Color for drawing traversal highlight
     * area when highlight is off. */
    XColor *highlightBgColorPtr;

    XColor *highlightColorPtr;	/* Color for drawing traversal highlight. */

    double nearclipping;
    RwV2d viewoffset;
    RwV2d viewwindow;

    char* projection;
    char* takeFocus;

    Tk_Cursor cursor;
    int flags;

    RwCamera* rwcamera;

    struct Camera* cam_next;   /* Next camera for scene (multiple cameras) */
    struct Scene* scene;       /* Current scene or NULL */
 } Camera;


typedef struct Model {
    Tcl_Interp* interp;
    Tcl_Command command;

    RwClump* rwclump;
    int npoly;                /* Number of polygons in list */
    RwPolygon3d* poly_first;  /* Polygon list via Data field */
    RwPolygon3d* poly_last;   /* Polygon list via Data field */
    struct Model* model_next; /* next model in top level list or NULL */
} Model;


typedef struct Scene {
    Tcl_Interp* interp;
    Tcl_Command command;

    RwScene* rwscene;
    Camera* cam_first;          /* Camera List */
    Model* model_first;  /* Model list (top level models only) */
} Scene;


typedef struct Light {
    Tcl_Interp* interp;
    Tcl_Command command;
    
    RwLight* rwlight;
} Light;


typedef struct Material {
    Tcl_Interp* interp;
    Tcl_Command command;
    RwMaterial* rwmaterial;
    struct Material* material_next;
} Material;


typedef struct Texture {
    int deleted;             /* 1 if deleted */
    int refc;                /* Reference count */
    Tcl_Interp* interp;
    Tcl_Command command;
    RwTexture* rwtexture;
    struct Texture* texture_next;
} Texture;



typedef enum {
    NONE,
    INTEGER,   /* int get(object), void* set(object, int) */
    REAL,      /* double get(object), void* set(object, double) */
    VECTOR2D,  /* void* get(object, struct), void* set(object, struct) */
    VECTOR3D,  /* void* get(object, struct), void* set(object, struct) */
    UV,        /* void* get(object, struct), void* set(object, struct) */
    COLOR,     /* void* get(object, struct), void* set(object, struct) */
    POINTER,
} TkRwDataType;


/* Option processing structure */

typedef int (*TkRwParseProc)(Tcl_Interp*, char*, void**);
typedef char* (*TkRwFormatProc)(void*, Tcl_FreeProc**, Tcl_Interp*);
typedef int (*TkRwSetProc)(void*, void*);
typedef void* (*TkRwGetProc)(void*);
typedef void* (*TkRwGetStructProc)(void*, void*);

typedef struct TkRwOption {
    TkRwDataType type;             /* Type of data */
    char* option;                  /* opion name */
    TkRwParseProc parse_proc;      /* parse proc */
    TkRwFormatProc format_proc;    /* format proc */
    TkRwSetProc set_proc;          /* set value proc */
    TkRwGetProc get_proc;          /* get value proc */
} TkRwOption;

typedef RPARAM (*TkRwGetRealProc)(void*);
typedef int (*TkRwSetRealProc)(void*, RPARAM);
typedef char* (*TkRwFormatRealProc)(RPARAM, Tcl_FreeProc**, Tcl_Interp*);

 /*
  * Flag bits for buttons:
  *
  * REDRAW_PENDING:		Non-zero means a DoWhenIdle handler
  *				has already been queued to redraw
  *				this window.
  * SELECTED:			Non-zero means this camera is selected,
  *				so special highlight should be drawn.
  * GOT_FOCUS:			Non-zero means this camera currently
  *				has the input focus.
  */

#define REDRAW_PENDING		1
#define SELECTED		2
#define GOT_FOCUS		4

#define BLACK		"Black"
#define WHITE		"White"

#define NORMAL_BG	"#d9d9d9"
#define ACTIVE_BG	"#ececec"
#define SELECT_BG	"#c3c3c3"
#define TROUGH		"#c3c3c3"
#define INDICATOR	"#b03060"
#define DISABLED	"#a3a3a3"

#define DEF_CAMERA_COLOR      "black"
#define DEF_CAMERA_IMAGE      (char*) NULL
#define DEF_CAMERA_CURSOR     ""
#define DEF_CAMERA_TAKE_FOCUS "0"
#define DEF_CAMERA_HEIGHT     "0"
#define DEF_CAMERA_WIDTH      "0"

#define DEF_CAMERA_BORDER_WIDTH    "0"
#define DEF_CAMERA_HIGHLIGHT_BG    NORMAL_BG
#define DEF_CAMERA_HIGHLIGHT       BLACK
#define DEF_CAMERA_HIGHLIGHT_WIDTH "0"
#define DEF_CAMERA_RELIEF          "flat"
#define DEF_CAMERA_STATE	"normal"

#define DEF_CAMERA_LOOKAT        "0.0 0.0 -1.0"
#define DEF_CAMERA_LOOKUP        "0.0 1.0 0.0"
#define DEF_CAMERA_LOOKRIGHT     (char*) NULL
#define DEF_CAMERA_NEARCLIPPING  "0.05"
#define DEF_CAMERA_PROJECTION    "perspective"
#define DEF_CAMERA_VIEWWINDOW    "1.0 1.0"
#define DEF_CAMERA_VIEWOFFSET    "0.0 0.0"
#define DEF_CAMERA_POSITION      "0.0 0.0 0.0"


#define OPT_LOOKAT       1
#define OPT_LOOKUP       2
#define OPT_LOOKRIGHT    3
#define OPT_POSITION     4

extern Tk_Uid			tkNormalUid;
extern Tk_Uid			tkDisabledUid;

static Tk_OptionParseProc Vector2DParseProc;
static Tk_OptionPrintProc  Vector2DPrintProc;

static Tk_OptionParseProc Vector3DParseProc;
static Tk_OptionPrintProc  Vector3DPrintProc;


static Tk_CustomOption lookatOption = {
    Vector3DParseProc,
    Vector3DPrintProc,
    (ClientData) OPT_LOOKAT
};

static Tk_CustomOption lookupOption = {
    Vector3DParseProc,
    Vector3DPrintProc,
    (ClientData) OPT_LOOKUP
};

static Tk_CustomOption lookrightOption = {
    Vector3DParseProc,
    Vector3DPrintProc,
    (ClientData) OPT_LOOKRIGHT
};

static Tk_CustomOption positionOption = {
    Vector3DParseProc,
    Vector3DPrintProc,
    (ClientData) OPT_POSITION
};

static Tk_CustomOption vector2DOption = {
    Vector2DParseProc,
    Vector2DPrintProc,
    (ClientData) 0
};


static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_COLOR, "-background", "background", "Background",
	 DEF_CAMERA_COLOR, Tk_Offset(Camera, backdrop_color)},

    {TK_CONFIG_SYNONYM, "-bg", "background", (char *) NULL,
	 (char *) NULL, 0},

    {TK_CONFIG_SYNONYM, "-bd", "borderWidth", (char *) NULL,
	 (char *) NULL, 0},

    {TK_CONFIG_PIXELS, "-borderwidth", "borderWidth", "BorderWidth",
	 DEF_CAMERA_BORDER_WIDTH, Tk_Offset(Camera, borderWidth)},

    {TK_CONFIG_ACTIVE_CURSOR, "-cursor", "cursor", "Cursor",
	 DEF_CAMERA_CURSOR, Tk_Offset(Camera, cursor),
	 TK_CONFIG_NULL_OK},

    {TK_CONFIG_PIXELS, "-height", "height", "Height",
	 DEF_CAMERA_HEIGHT, Tk_Offset(Camera, height), 0},

    {TK_CONFIG_COLOR, "-highlightbackground", "highlightBackground",
	 "HighlightBackground", 
	 DEF_CAMERA_HIGHLIGHT_BG, Tk_Offset(Camera, highlightBgColorPtr)},

    {TK_CONFIG_COLOR, "-highlightcolor", "highlightColor", "HighlightColor",
	 DEF_CAMERA_HIGHLIGHT, Tk_Offset(Camera, highlightColorPtr)},

    {TK_CONFIG_PIXELS, "-highlightthickness", "highlightThickness",
	 "HighlightThickness",
	 DEF_CAMERA_HIGHLIGHT_WIDTH, Tk_Offset(Camera, highlightWidth)},

    {TK_CONFIG_RELIEF, "-relief", "relief", "Relief",
	 DEF_CAMERA_RELIEF, Tk_Offset(Camera, relief)},

    {TK_CONFIG_UID, "-state", "state", "State",
	 DEF_CAMERA_STATE, Tk_Offset(Camera, state), 0},

    {TK_CONFIG_STRING, "-backdrop", "backdrop", "Backdrop",
	 DEF_CAMERA_IMAGE, Tk_Offset(Camera, backdrop_name),
	 TK_CONFIG_NULL_OK},

    {TK_CONFIG_CUSTOM, "-lookat", (char*) NULL, (char*) NULL,
	 DEF_CAMERA_LOOKAT, -1,
	 TK_CONFIG_NULL_OK, &lookatOption},

    {TK_CONFIG_CUSTOM, "-lookup", (char*) NULL, (char*) NULL,
	 DEF_CAMERA_LOOKUP, -1, 
	 TK_CONFIG_NULL_OK, &lookupOption},

    {TK_CONFIG_CUSTOM, "-lookright", (char*) NULL, (char*) NULL,
	 DEF_CAMERA_LOOKRIGHT, -1,
	 TK_CONFIG_NULL_OK, &lookrightOption},

    {TK_CONFIG_CUSTOM, "-position", (char*) NULL, (char*) NULL,
	 DEF_CAMERA_POSITION, -1,
	 TK_CONFIG_NULL_OK, &positionOption},

    {TK_CONFIG_DOUBLE, "-nearclipping", (char*) NULL, (char*) NULL,
	 DEF_CAMERA_NEARCLIPPING, Tk_Offset(Camera, nearclipping),
	 0 },

    {TK_CONFIG_STRING, "-projection", (char*) NULL, (char*) NULL,
	 DEF_CAMERA_PROJECTION, Tk_Offset(Camera, projection),
	 0 },

    {TK_CONFIG_STRING, "-takefocus", "takeFocus", "TakeFocus",
	 DEF_CAMERA_TAKE_FOCUS, Tk_Offset(Camera, takeFocus),
	 TK_CONFIG_NULL_OK},

    {TK_CONFIG_CUSTOM, "-viewoffset", (char*) NULL, (char*) NULL,
	 DEF_CAMERA_VIEWOFFSET, Tk_Offset(Camera, viewoffset),
	 TK_CONFIG_NULL_OK, &vector2DOption},

    {TK_CONFIG_CUSTOM, "-viewwindow", (char*) NULL, (char*) NULL,
	 DEF_CAMERA_VIEWWINDOW, Tk_Offset(Camera, viewwindow),
	 TK_CONFIG_NULL_OK, &vector2DOption},

    {TK_CONFIG_PIXELS, "-width", "width", "Width",
	 DEF_CAMERA_WIDTH, Tk_Offset(Camera, width), 0},

    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
	 (char *) NULL, 0, 0}
};

extern void free _ANSI_ARGS_ ((void*));

static void CameraCmdDeletedProc _ANSI_ARGS_((ClientData clientData));

static void CameraEventProc _ANSI_ARGS_((ClientData clientData,
					 XEvent *eventPtr));
static void DisplayCamera _ANSI_ARGS_((ClientData clientData));

static int  CameraWidgetCmd _ANSI_ARGS_((ClientData clientData,
					 Tcl_Interp *interp, 
					 int argc, char **argv));
static int  ConfigureCamera _ANSI_ARGS_((Tcl_Interp *interp,
					 Camera *camPtr, 
					 int argc, char **argv,
					 int));

static int SceneCmd _ANSI_ARGS_((ClientData clientData,
				 Tcl_Interp *interp, 
				 int argc, char **argv));
static void SceneDeletedProc _ANSI_ARGS_((ClientData clientData));


static int ModelCmd _ANSI_ARGS_((ClientData clientData,
				 Tcl_Interp *interp, 
				 int argc, char **argv));

static void ModelDeletedProc _ANSI_ARGS_((ClientData clientData));


static void DestroyModel _ANSI_ARGS_ ((Model* model));


static int MaterialCmd _ANSI_ARGS_((ClientData clientData,
				    Tcl_Interp *interp, 
				    int argc, char **argv));

static void MaterialDeletedProc _ANSI_ARGS_((ClientData clientData));


static int TextureCmd _ANSI_ARGS_((ClientData clientData,
				    Tcl_Interp *interp, 
				    int argc, char **argv));

static void TextureDeletedProc _ANSI_ARGS_((ClientData clientData));


static int  ConfigureScene _ANSI_ARGS_((Tcl_Interp *interp,
					Scene *scene,
					int argc, char **argv));

static int LightCmd _ANSI_ARGS_((ClientData clientData,
				 Tcl_Interp *interp, 
				 int argc, char **argv));
static void LightDeletedProc _ANSI_ARGS_((ClientData clientData));

static void DestroyLight _ANSI_ARGS_ ((Light* light));

static Scene* FindScene _ANSI_ARGS_ ((Tcl_Interp* interp, char* name));

static void RemoveSceneCamera _ANSI_ARGS_ ((Scene* scene, Camera* camera));
static void AddSceneCamera _ANSI_ARGS_ ((Scene* scene, Camera* camera));
static void InvalidateScene _ANSI_ARGS_ ((Scene* scene));

static Material* FindMaterial _ANSI_ARGS_ ((RwMaterial*));
static void AddMaterial _ANSI_ARGS_ ((Material* mp));
static void RemoveMaterial _ANSI_ARGS_ ((Material* mp));

static Texture* FindTexture _ANSI_ARGS_ ((RwTexture*));
static void AddTexture _ANSI_ARGS_ ((Texture* tp));
static void RemoveTexture _ANSI_ARGS_ ((Texture* tp));
static void DestroyTexture  _ANSI_ARGS_ ((Texture* tp));
static void ReferenceTexture  _ANSI_ARGS_ ((RwTexture* tex));
static void DereferenceTexture  _ANSI_ARGS_ ((RwTexture* tex));


static void RenderSceneCamera _ANSI_ARGS_ ((Scene* scene, Camera* camera));
static void RenderScene _ANSI_ARGS_ ((Scene* scene));
static void RenderCamera _ANSI_ARGS_ ((Camera* camera));

#define CAMERA_MAX_VIEW_WIDTH  1024
#define CAMERA_MAX_VIEW_HEIGHT 900

/* Globals */

static X11DevParams rw_params;
static Tcl_HashTable rw_xcolors;
static RwMatrix4d* rw_matrix1;
static RwMatrix4d* rw_matrix2;
static Material* rw_material_list = NULL;
static Texture* rw_texture_list = NULL;

#define RGB_COLOR_MAX  255.0
#define XRGB_COLOR_MAX 65535.0

static char rw_fmtbuf[1024];

/*
** Calculate the vector distantce
*/
double VectorNorm(x, y, z)
double x, y, z;
{
    return sqrt(x*x + y*y + z*z);
}

/*
** Build a orientation matrix, i.e abolue angle orientation
** around x (ax) and y (ay) and z (az)
*/
RwMatrix4d* OrientMatrix(mat, ax, ay, az)
RwMatrix4d* mat; double ax; double ay; double az;
{
    RwReal m[4][4];

    RwGetMatrixElements(mat, m);

    /* Set scale matrix */
    RwScaleMatrix(mat,
		  VectorNorm(FL2REAL(m[0][0]),
			     FL2REAL(m[0][1]),
			     FL2REAL(m[0][2])),
		  VectorNorm(FL2REAL(m[1][0]), 
			     FL2REAL(m[1][1]),
			     FL2REAL(m[1][2])),
		  VectorNorm(FL2REAL(m[2][0]), 
			     FL2REAL(m[2][1]),
			     FL2REAL(m[2][2])),
		  rwREPLACE);
    /* Rotate */
    RwRotateMatrix(mat, 1.0, 0.0, 0.0, ax, rwPRECONCAT);
    RwRotateMatrix(mat, 0.0, 1.0, 0.0, ay, rwPRECONCAT);
    RwRotateMatrix(mat, 0.0, 0.0, 1.0, az, rwPRECONCAT);

    /* Restore translation */
    RwSetMatrixElement(mat, 3, 0, FL2REAL(m[3][0]));
    RwSetMatrixElement(mat, 3, 1, FL2REAL(m[3][1]));
    RwSetMatrixElement(mat, 3, 2, FL2REAL(m[3][2]));

    return mat;
}

/*
**
*/
void XColor2RwColor(xcolor, rwcolor)
    XColor* xcolor; RwRGBColor* rwcolor;
{
    rwcolor->r = FL2REAL(xcolor->red/XRGB_COLOR_MAX);
    rwcolor->g = FL2REAL(xcolor->green/XRGB_COLOR_MAX);
    rwcolor->b = FL2REAL(xcolor->blue/XRGB_COLOR_MAX);
}

/*
 ** Given a color name find the RGB entry
 */
static XColorEntry* FindRwColor(name)
    char* name;
{
    Tcl_HashEntry* h;

    if ((h = Tcl_FindHashEntry(&rw_xcolors, name)) == NULL)
	return NULL;
    return (XColorEntry*) Tcl_GetHashValue(h);
}

/*
 **
 */

static int Scan3Double(interp, argv, x, y, z)
    Tcl_Interp* interp; char** argv; 
    double* x; double* y; double *z;
{
    if (Tcl_GetDouble(interp, argv[0], x) != TCL_OK)
	return TCL_ERROR;
    if (Tcl_GetDouble(interp, argv[1], y) != TCL_OK)
	return TCL_ERROR;
    if (Tcl_GetDouble(interp, argv[2], z) != TCL_OK)
	return TCL_ERROR;
    return TCL_OK;
}


static int ParseRwVector3D(interp, value, vector)
    Tcl_Interp* interp; char* value; RwV3d* vector;
{
    double x;
    double y;
    double z;
    int argc;
    char** argv;

    if (Tcl_SplitList(interp, value, &argc, &argv) != TCL_OK)
	return TCL_ERROR;

    if (argc != 3) {
	Tcl_AppendResult(interp, "expected 3D vector but got \"",
			 value, "\"", (char*) NULL);
	goto error;
    }

    if (Scan3Double(interp, argv, &x, &y, &z) != TCL_OK)
	goto error;

    vector->x = FL2REAL(x);
    vector->y = FL2REAL(y);
    vector->z = FL2REAL(z);
    ckfree(argv);
    return TCL_OK;
 error:
    ckfree(argv);
    return TCL_ERROR;
}


static char* FormatRwVector3D(vector, freeProcPtr, interp)
    RwV3d* vector; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    sprintf(rw_fmtbuf, "%g %g %g", 
	    REAL2FL(vector->x), 
	    REAL2FL(vector->y),
	    REAL2FL(vector->z));
    return rw_fmtbuf;
}


static int ParseRwVector2D(interp, value, vector)
    Tcl_Interp* interp; char* value; RwV2d* vector;
{
    double x;
    double y;
    int argc;
    char** argv;

    if (Tcl_SplitList(interp, value, &argc, &argv) != TCL_OK)
	return TCL_ERROR;

    if (argc != 2) {
	Tcl_AppendResult(interp, "expected 2D vector but got \"",
			 value, "\"", (char*) NULL);
	goto error;
    }
    if (Tcl_GetDouble(interp, argv[0], &x) != TCL_OK)
	goto error;
    if (Tcl_GetDouble(interp, argv[1], &y) != TCL_OK)
	goto error;

    vector->x = FL2REAL(x);
    vector->y = FL2REAL(y);
    ckfree(argv);
    return TCL_OK;
 error:
    ckfree(argv);
    return TCL_ERROR;
}


static char* FormatRwVector2D(vector, freeProcPtr, interp)
    RwV2d* vector; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    sprintf(rw_fmtbuf, "%g %g", 
	    REAL2FL(vector->x), 
	    REAL2FL(vector->y));
    return rw_fmtbuf;
}


static int ParseRwUV(interp, value, vector)
    Tcl_Interp* interp; char* value; RwUV* vector;
{
    double u;
    double v;
    int argc;
    char** argv;

    if (Tcl_SplitList(interp, value, &argc, &argv) != TCL_OK)
	return TCL_ERROR;

    if (argc != 2) {
	Tcl_AppendResult(interp, "expected 2D vector but got \"",
			 value, "\"", (char*) NULL);
	goto error;
    }
    if (Tcl_GetDouble(interp, argv[0], &u) != TCL_OK)
	goto error;
    if (Tcl_GetDouble(interp, argv[1], &v) != TCL_OK)
	goto error;

    vector->u = FL2REAL(u);
    vector->v = FL2REAL(v);
    ckfree(argv);
    return TCL_OK;
 error:
    ckfree(argv);
    return TCL_ERROR;
}


static char* FormatRwUV(vector, freeProcPtr, interp)
    RwUV* vector; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    sprintf(rw_fmtbuf, "%g %g", 
	    REAL2FL(vector->u), 
	    REAL2FL(vector->v));
    return rw_fmtbuf;
}


static int ParseRwInt(interp, value, x)
    Tcl_Interp* interp; char* value; int* x;
{
    return Tcl_GetInt(interp, value, x);
}

static char* FormatRwInt(x, freeProcPtr, interp)
    int x; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    sprintf(rw_fmtbuf, "%d", x);
    return rw_fmtbuf;
}


static int ParseRwReal(interp, value, x)
    Tcl_Interp* interp; char* value; RPARAM* x;
{
    if (Tcl_GetDouble(interp, value, x) != TCL_OK)
	return TCL_ERROR;
    return TCL_OK;
}


static char* FormatRwReal(x, freeProcPtr, interp)
    RPARAM x; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    sprintf(rw_fmtbuf, "%g", x);
    return rw_fmtbuf;
}

static int ParseRwState(interp, value, x)
    Tcl_Interp* interp; char* value; RwState* x;
{
    if (strcmp(value, "on") == 0)
	*x = rwON;
    else if (strcmp(value, "off") == 0)
	*x = rwOFF;
    else {
	Tcl_AppendResult(interp,
			 "expected on or off but got \"", 
			 value,
			 "\"", (char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}


static char* FormatRwState(x, freeProcPtr, interp)
    RwState x; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    if (x == rwOFF)
	return "off";
    else if (x == rwON)
	return "on";
    else
	return "????";
}


static int ParseRwAlignAxis(interp, value, x)
    Tcl_Interp* interp; char* value; RwAxisAlignment* x;
{
    if (strcmp(value, "zx") == 0)
	*x = rwALIGNAXISZORIENTX;
    else if (strcmp(value, "zy") == 0)
	*x = rwALIGNAXISZORIENTY;
    else if (strcmp(value, "xyz") == 0)
	*x = rwALIGNAXISXYZ;
    else if (strcmp(value, "none") == 0)
	*x = rwNOAXISALIGNMENT;
    else {
	Tcl_AppendResult(interp,
			 "expected x, y, xyz or none but got \"", 
			 value,
			 "\"", (char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}


static char* FormatRwAlignAxis(x, freeProcPtr, interp)
    RwAxisAlignment x; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    switch(x) {
    case rwALIGNAXISZORIENTX: return "zx";
    case rwALIGNAXISZORIENTY: return "zy";
    case rwALIGNAXISXYZ: return "xyz";
    case rwNOAXISALIGNMENT:
    default:
	return "????";
    }
}

static int ParseRwGeometrySampling(interp, value, x)
    Tcl_Interp* interp; char* value; RwGeometrySampling* x;
{
    if (strcmp(value, "cloud") == 0)
	*x = rwPOINTCLOUD;
    else if (strcmp(value, "wireframe") == 0)
	*x = rwWIREFRAME;
    else if (strcmp(value, "solid") == 0)
	*x = rwSOLID;
    else {
	Tcl_AppendResult(interp,
			 "expected cloud, wireframe or solid but got \"",
			 value,
			 "\"", (char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

static char* FormatRwGeometrySampling(x, freeProcPtr, interp)
RwGeometrySampling x; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    if (x == rwPOINTCLOUD)
	return "cloud";
    else if (x == rwWIREFRAME)
	return "wireframe";
    else if (x == rwSOLID)
	return "solid";
    else
	return "????";
}

static int ParseRwLightSampling(interp, value, x)
    Tcl_Interp* interp; char* value; RwLightSampling* x;
{
    if (strcmp(value, "facet") == 0)
	*x = rwFACET;
    else if (strcmp(value, "vertex") == 0)
	*x = rwVERTEX;
    else {
	Tcl_AppendResult(interp,
			 "expected facet or vertex but got \"",
			 value,
			 "\"", (char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

static char* FormatRwLightSampling(x, freeProcPtr, interp)
RwLightSampling x; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    if (x == rwFACET)
	return "facet";
    else if (x == rwVERTEX)
	return "vertex";
    else
	return "????";
}

static int ParseRwTextureModes(interp, value, x)
    Tcl_Interp* interp; char* value; RwTextureModes* x;
{
    RwTextureModes modes = 0;
    char* p = value;

    while(*p == ' ') p++;
    while(*p != '\0') {
	if (strncmp(p, "lit", 3) == 0) {
	    p += 3;
	    modes |= rwLIT;
	}
	else if (strncmp(p, "foreshorten", 11) == 0) {
	    p += 11;
	    modes |= rwFORESHORTEN;
	}
	else
	    goto error;
	while(*p == ' ') p++;
    }
    *x = modes;
    return TCL_OK;

 error:
    Tcl_AppendResult(interp,
		     "expected lit and/or foreshorten but got \"",
		     value,
		     "\"", (char *) NULL);
    return TCL_ERROR;
}

static char* FormatRwTextureModes(x, freeProcPtr, interp)
RwTextureModes x; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    if ((x & (rwLIT | rwFORESHORTEN)) == (rwLIT | rwFORESHORTEN))
	return "lit foreshorten";
    else if (x & rwLIT)
	return "lit";
    else if (x & rwFORESHORTEN)
	return "foreshorten";
    else
	return "";
}


static int ParseRwMaterial(interp, value, x)
Tcl_Interp* interp; char* value; RwMaterial** x;
{
    Tcl_CmdInfo info;
    Material* mp;

    if (!Tcl_GetCommandInfo(interp, value, &info) ||
	info.proc != MaterialCmd) {
	Tcl_AppendResult(interp, "material \"", value, 
			 "\" does not exist", (char*) NULL);
	return TCL_ERROR;
    }
    mp = (Material*) info.clientData;
    *x = mp->rwmaterial;
    return TCL_OK;
}


static char* FormatRwMaterial(x, freeProcPtr, interp)
RwMaterial* x; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    Material* mp = FindMaterial(x);

    if (mp != NULL)
	return Tcl_GetCommandName(mp->interp, mp->command);
    return "";
}


static int ParseRwTexture(interp, value, x)
Tcl_Interp* interp; char* value; RwTexture** x;
{
    Tcl_CmdInfo info;
    Texture* tp;

    if (*value == '\0') {
	*x = NULL;
	return TCL_OK;
    }
    if (!Tcl_GetCommandInfo(interp, value, &info) ||
	info.proc != TextureCmd) {
	Tcl_AppendResult(interp, "texture \"", value, 
			 "\" does not exist", (char*) NULL);
	return TCL_ERROR;
    }
    tp = (Texture*) info.clientData;
    *x = tp->rwtexture;
    return TCL_OK;
}


static char* FormatRwTexture(x, freeProcPtr, interp)
RwTexture* x; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    Texture* tp = FindTexture(x);

    if (tp != NULL) 
	return Tcl_GetCommandName(tp->interp, tp->command);
    return "";
}


static int ParseRwRasterFile(interp, value, x)
Tcl_Interp* interp; char* value; RwRaster** x;
{
    RwRaster* ras;
    char* name;
    RwRasterOptions opts = rwFITRASTER | rwAUTODITHERRASTER | rwGAMMARASTER;

    if ((ras = RwReadRaster(value, opts)) == NULL) {
	Tcl_AppendResult(interp, "file \"", value, 
			 "\" does not exist or bad format", (char*) NULL);
	return TCL_ERROR;
    }
    name = (char*) ckalloc(strlen(value) + 1);
    strcpy(name, value);
    RwSetRasterData(ras, (void*) name);  /* DONT FORGET TO FREE */
    *x = ras;
    return TCL_OK;
}


static char* FormatRwRasterFile(x, freeProcPtr, interp)
RwRaster* x; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    char* name = (char*) RwGetRasterData(x);

    if (name != NULL)
	return name;
    return "";
}

/*
** This routine should be able to read different file formats
** including GIF/TIFF/PPM/RAS ...
** But now it only read RasterFile with the normal RwReadRaster
*/
static RwRaster* LoadRaster(interp, name)
Tcl_Interp* interp; char* name;
{
    RwRasterOptions opts = rwGAMMARASTER;
    RwRaster* ras;

    if ((ras = RwReadRaster(name, opts)) == NULL) {
	Tcl_AppendResult(interp, "file \"", name, 
			 "\" does not exist or bad format", (char*) NULL);
	return NULL;
    }
    return ras;
}


#define MAX_COLOR_NAME_LENGTH 30
/*
 ** Name is converted
 */
static int ParseRwColor(interp, value, rwcolor)
    Tcl_Interp* interp; char* value; RwRGBColor* rwcolor;
{
    int length = strlen(value);

    if (length >= 30)
	goto error;

    if (value[0] == '#') {
	int rgb_len;
	unsigned long rgb[3];
	double rgb_max;
	char* p = value + 1;
	int i;
	int j;

	if (length == 7) 	/* #RRGGBB */
	    rgb_len = 2;
	else if (length == 10)  /* #RRRGGGBBB */
	    rgb_len = 3;
	else if (length == 13)  /* #RRRRGGGGBBBB */
	    rgb_len = 4;
	else 
	    goto error;

	rgb_max = (1 << (rgb_len*4));

	for (i = 0; i < 3; i++) {
	    unsigned long val = 0;

	    for (j = 0; j < rgb_len; j++) {
		int c = *p++;
		if (isxdigit(c)) {
		    if (isdigit(c))
			val = val*16 + (c - '0');
		    else if (islower(c)) 
			val = val*16 + 10 + (c - 'a');
		    else
			val = val*16 + 10 + (c - 'A');
		}
		else
		    goto error;
	    }
	    rgb[i] = val;
	}
	rwcolor->r = FL2REAL(rgb[0] / rgb_max);
	rwcolor->g = FL2REAL(rgb[1] / rgb_max);
	rwcolor->b = FL2REAL(rgb[2] / rgb_max);
	return TCL_OK;
    }
    else if (isalpha(value[0])) {
	char colorbuf[MAX_COLOR_NAME_LENGTH];
	char* p;
	char* q;
	XColorEntry* cp;

	p = colorbuf;
	q = value;
	while(*q != '\0') {
	    *p++ = tolower(*q);
	    q++;
	}
	*p = '\0';

	if ((cp = FindRwColor(colorbuf)) == NULL)
	    goto error;

	rwcolor->r = FL2REAL(cp->red / RGB_COLOR_MAX);
	rwcolor->g = FL2REAL(cp->green / RGB_COLOR_MAX);
	rwcolor->b = FL2REAL(cp->blue / RGB_COLOR_MAX);

	return TCL_OK;
    }
    else {
	RwV3d vector;

	if (ParseRwVector3D(interp, value, vector) == TCL_OK) {
	    rwcolor->r = vector.x;
	    rwcolor->g = vector.y;
	    rwcolor->b = vector.z;
	    return TCL_OK;
	}
	Tcl_ResetResult(interp); /* Reset error from ParseRwVector */
    }
 error:
    Tcl_AppendResult(interp, "expected a color but got \"", value,
		     "\"", (char*) NULL);
    return TCL_ERROR;
}

/*
** Consider to format names as well !
*/
static char* FormatRwColor(x, freeProcPtr, interp)
RwRGBColor* x; Tcl_FreeProc** freeProcPtr; Tcl_Interp* interp;
{
    int r = x->r * RGB_COLOR_MAX;
    int g = x->g * RGB_COLOR_MAX;
    int b = x->b * RGB_COLOR_MAX;

    sprintf(rw_fmtbuf, "#%02x%02x%02x", r, g, b);
    return rw_fmtbuf;
}


/*
** Find config spec entry from option
*/
TkRwOption* TkRwFindSpec(interp, sp, opt)
Tcl_Interp* interp;  TkRwOption* sp; char* opt;
{
    char* specopt;

    while((specopt = sp->option) != NULL && 
	  (strcmp(specopt, opt) != 0))
	sp++;
    if (specopt != NULL)
	return sp;
    Tcl_AppendResult(interp,
		     "unknown option \"", opt, "\"", (char*) NULL);
    return NULL;
}


/*
** Set an option value
**
*/

static int TkRwSetSpec(interp, object, sp, opt, val)
Tcl_Interp* interp; void* object; TkRwOption* sp;
char* opt; char* val;
{
    TkRwSetProc sf = sp->set_proc;

    if (sf == NULL) {
	Tcl_AppendResult(interp, "option \"", opt, "\" not writeable", NULL);
	return TCL_ERROR;
    }

    switch(sp->type) {
    case INTEGER: {
	int value;

	if ((*sp->parse_proc)(interp,val,(void*)&value) != TCL_OK)
	    return TCL_ERROR;
	if ((*sf)(object, (void*) value) != 0)
	    return TCL_OK;
	break;
    }

    case REAL: {
	RPARAM value;

	if ((*sp->parse_proc)(interp, val, (void*)&value) != TCL_OK)
	    return TCL_ERROR;
	if ( (*(TkRwSetRealProc)sf)(object, value) != 0)
	    return TCL_OK;
	break;
    }

    case UV: {
	RwUV value;

	if ((*sp->parse_proc)(interp,val,(void*)&value) != TCL_OK)
	    return TCL_ERROR;
	if ((*sf)(object, (void*) &value) != 0)
	    return TCL_OK;
	break;
    }

    case VECTOR2D: {
	RwV2d value;

	if ((*sp->parse_proc)(interp,val,(void*)&value) != TCL_OK)
	    return TCL_ERROR;
	if ((*sf)(object, (void*) &value) != 0)
	    return TCL_OK;
	break;
    }


    case VECTOR3D: {
	RwV3d value;

	if ((*sp->parse_proc)(interp,val,(void*)&value) != TCL_OK)
	    return TCL_ERROR;
	if ((*sf)(object, (void*) &value) != 0)
	    return TCL_OK;
	break;
    }

    case COLOR: {
	RwRGBColor value;

	if ((*sp->parse_proc)(interp,val,(void*)&value) != TCL_OK)
	    return TCL_ERROR;
	if ((*sf)(object, (void*) &value) != 0)
	    return TCL_OK;
	break;
    }

    case POINTER: {
	void* value;
	
	if ((*sp->parse_proc)(interp,val, &value) != TCL_OK)
	    return TCL_ERROR;
	if ((*sf)(object, value) != 0)
	    return TCL_OK;
	break;
    }

    default:
	Tcl_AppendResult(interp,
			 "internal error in TkRwSetSpec", 
			 (char*) NULL);
	return TCL_ERROR;
    }

    Tcl_AppendResult(interp,
		     "bad value given to option \"", opt, "\"", 
		     (char*) NULL);
    return TCL_ERROR;    
}

/*
** Get an option value
*/

static int TkRwGetSpec(interp, object, sp, opt)
Tcl_Interp* interp; void* object; TkRwOption* sp; char* opt;
{
    Tcl_FreeProc* freeProc = NULL;
    TkRwGetProc gf = sp->get_proc;
    char* ptr;

    if (gf == NULL) {
	Tcl_AppendResult(interp, "option \"", opt, "\" not readable", NULL);
	return TCL_ERROR;
    }

    switch(sp->type) {
    case INTEGER: {
	int value;

	value = (int) (*gf)(object);
	if ((ptr = (*sp->format_proc)((void*)value,&freeProc,interp)) != NULL)
	    Tcl_AppendResult(interp, ptr, (char*) NULL);
	break;
    }

    case REAL: {
	RPARAM value;

	value = (*(TkRwGetRealProc)gf)(object);
	if ((ptr = (*(TkRwFormatRealProc)sp->format_proc)(value, &freeProc, interp)) != NULL)
	    Tcl_AppendResult(interp, ptr, (char*) NULL);
	break;	
    }

    case UV: {
	RwUV value;

	(*(TkRwGetStructProc)gf)(object, (void*)&value);

	if ((ptr = (*sp->format_proc)((void*)&value,&freeProc,interp)) != NULL)
	    Tcl_AppendResult(interp, ptr, (char*) NULL);
	break;
    }

    case VECTOR2D: {
	RwV2d value;

	(*(TkRwGetStructProc)gf)(object, (void*)&value);

	if ((ptr = (*sp->format_proc)((void*)&value,&freeProc,interp)) != NULL)
	    Tcl_AppendResult(interp, ptr, (char*) NULL);
	break;	
    }

    case VECTOR3D: {
	RwV3d value;

	(*(TkRwGetStructProc)gf)(object, (void*)&value);

	if ((ptr = (*sp->format_proc)((void*)&value,&freeProc,interp)) != NULL)
	    Tcl_AppendResult(interp, ptr, (char*) NULL);
	break;	
    }

    case COLOR: {
	RwRGBColor value;

	(*(TkRwGetStructProc)gf)(object,(void*)&value);

	if ((ptr = (*sp->format_proc)((void*)&value,&freeProc,interp)) != NULL)
	    Tcl_AppendResult(interp, ptr, (char*) NULL);
	break;
    }

    case POINTER: {
	void* value;

	value = (*gf)(object);
	if ((ptr = (*sp->format_proc)(value, &freeProc,interp)) != NULL)
	    Tcl_AppendResult(interp, ptr, (char*) NULL);
	break;
    }

    default:
	Tcl_AppendResult(interp,
			 "internal error in TkRwGetSpec", 
			 (char*) NULL);
	return TCL_ERROR;
    }

    if (ptr == NULL) {
	Tcl_AppendResult(interp, "invalid", (char*) NULL);
	return TCL_OK;
    }
#ifdef DEBUG
    fprintf(stderr, "VALUE = %s", ptr);
#endif

    if (freeProc != NULL) {
	if ((freeProc == TCL_DYNAMIC) || (freeProc == (Tcl_FreeProc *)free)) {
	    ckfree(ptr);
	} else {
	    (*freeProc)(ptr);
	}
    }
    return TCL_OK;
}


/*
** Configure an object from argv 
*/

int TkRwConfigure(interp, object, specs, argc, argp)
Tcl_Interp* interp; void* object; TkRwOption* specs;
int argc; char** argp;
{
    char* opt;
    char* val;

    while(argc >= 2) {
	TkRwOption* sp;
	int res;

	argc -= 2;
	opt = *argp++;

	if ((sp = TkRwFindSpec(interp, specs, opt)) == NULL)
	    return TCL_ERROR;

	val = *argp++;

#ifdef DEBUG
	fprintf(stderr, "CONFIG OPTION %s to %s ", opt, val);
#endif
	if (TkRwSetSpec(interp, object, sp, opt, val) == TCL_ERROR)
	    return TCL_ERROR;
#ifdef DEBUG
	TkRwGetSpec(interp, object, sp, opt);
	fprintf(stderr, "\n\r");
	Tcl_ResetResult(interp);
#endif
    }
    if (argc == 0)
	return TCL_OK;

    Tcl_AppendResult(interp,
		     "no value given to option \"", *argp, "\"", 
		     (char*) NULL);
    return TCL_ERROR;
}

/*
** Format an option value for object 
*/
int TkRwConfigureInfo(interp, object, specs, opt)
Tcl_Interp* interp; void* object; TkRwOption* specs; char* opt;
{
    Tcl_SetResult(interp, (char *) NULL, TCL_STATIC);

    if (opt != NULL) {
	TkRwOption* sp;

	if ((sp = TkRwFindSpec(interp, specs, opt)) == NULL)
	    return TCL_ERROR;

	return TkRwGetSpec(interp, object, sp, opt);
    }
    else {
	while(specs->option != NULL) {
	    Tcl_AppendResult(interp, "{", specs->option, " ", (char*) NULL);
	    TkRwGetSpec(interp, object, specs, specs->option);
	    Tcl_AppendResult(interp, "} ", (char*) NULL);
	    specs++;
	}
	return TCL_OK;
    }
}


/* Camera widget Wrapers */

int Vector3DParseProc(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;              /* Not used.*/
    Tcl_Interp *interp;                 /* Used for reporting errors. */
    Tk_Window tkwin;                    /* Window containing canvas widget. */
    char *value;                        /* Value of option (list of tag
                                         * names). */
    char *widgRec;                      /* Pointer to record for item. */
    int offset;                         /* Offset into item */
{
    if (offset == -1) {
	RwV3d vector;
	Camera* camera = (Camera*) widgRec;
	int opt = (int) clientData;

	if (ParseRwVector3D(interp, value, &vector) != TCL_OK)
	    return TCL_ERROR;
	switch((int) clientData) {
	case OPT_LOOKAT:
	    RwSetCameraLookAt(camera->rwcamera, 
			      vector.x, vector.y, vector.z);
	    break;
	case OPT_LOOKUP:
	    RwSetCameraLookUp(camera->rwcamera,
			      vector.x, vector.y, vector.z);
	    break;

	case OPT_POSITION:
	    RwSetCameraPosition(camera->rwcamera,
				vector.x, vector.y, vector.z);
	    break;

	case OPT_LOOKRIGHT:
	    sprintf(interp->result, "lookright option is readonly");
	    return TCL_ERROR;

	default:
	    sprintf(interp->result, "bad config table: unknown type %d",
		    opt);
	    return TCL_ERROR;
	}
	return ParseRwVector3D(interp, value, &vector);
    }
    else
	return ParseRwVector3D(interp, value, (RwV3d*)(widgRec + offset));
}

char *Vector3DPrintProc(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;              /* Ignored. */
    Tk_Window tkwin;                    /* Window containing canvas widget. */
    char *widgRec;                      /* Pointer to record for item. */
    int offset;                         /* Ignored. */
    Tcl_FreeProc **freeProcPtr;         /* Pointer to variable to fill in with
                                         * information about how to reclaim
                                         * storage for return string. */
{
    if (offset == -1) {
	RwV3d vector;
	Camera* camera = (Camera*) widgRec;
	int opt = (int) clientData;

	switch((int) clientData) {
	case OPT_LOOKAT:
	    RwGetCameraLookAt(camera->rwcamera, &vector);
	    break;
	case OPT_LOOKUP:
	    RwGetCameraLookUp(camera->rwcamera, &vector);
	    break;
	case OPT_LOOKRIGHT:
	    RwGetCameraLookRight(camera->rwcamera, &vector);
	    break;
	case OPT_POSITION:
	    RwGetCameraPosition(camera->rwcamera, &vector);
	    break;
	}
	return FormatRwVector3D(&vector, freeProcPtr, NULL);
    }
    else
	return FormatRwVector3D((void*) (widgRec + offset), freeProcPtr, NULL);
}



int Vector2DParseProc(clientData, interp, tkwin, value, widgRec, offset)
    ClientData clientData;              /* Not used.*/
    Tcl_Interp *interp;                 /* Used for reporting errors. */
    Tk_Window tkwin;                    /* Window containing canvas widget. */
    char *value;                        /* Value of option (list of tag
                                         * names). */
    char *widgRec;                      /* Pointer to record for item. */
    int offset;                         /* Offset into item */
{
    return ParseRwVector2D(interp, value, (void*) (widgRec + offset));
}


char *Vector2DPrintProc(clientData, tkwin, widgRec, offset, freeProcPtr)
    ClientData clientData;              /* Ignored. */
    Tk_Window tkwin;                    /* Window containing canvas widget. */
    char *widgRec;                      /* Pointer to record for item. */
    int offset;                         /* Ignored. */
    Tcl_FreeProc **freeProcPtr;         /* Pointer to variable to fill in with
                                         * information about how to reclaim
                                         * storage for return string. */
{
    return FormatRwVector2D((void*) (widgRec + offset), freeProcPtr, NULL);
}


/*
** CameraCmd:
** 
**   Name: pathName [<maxwidth> <maxheight>] options
**
**   Options:
**         -width <integer>
**         -height <integer>
**         -cursor <Bitmap>
**         -background <color>
**         -bg <color>           # backdrop color
**         -backdrop <file>      # backdrop image name
**         -lookat {<x> <y> <z>}
**         -lookup {<x> <y> <z>}
**         -nearclipping Near
**         -projection <parallel | perspective>
**         -viewoffset {<X> <y>}
**         -viewwindow {<width> <height>}
**         -position {<x> <y> <z>}
**
**
**   Commands:
**         configure ?options?
**         cget option
**         render <scene>
**         tilt <angle>
**         revolve <angle>
**         pan <angle>
**         rotate Vx Vy Vz angle 
**         rotate Ax Ay Az         (absolute orientation)
**         move  <dx> <dy> <dz>
**
**
*/
int Tk_CameraCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    register Camera *camPtr;
    Tk_Window tkwin = (Tk_Window) clientData;
    Tk_Window new;
    RwCamera* rwcamera;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " pathName ?options?\"", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Create the new window.
     */

    new = Tk_CreateWindowFromPath(interp, tkwin, argv[1], (char *) NULL);
    if (new == NULL) {
	return TCL_ERROR;
    }

    camPtr = (Camera*) ckalloc(sizeof(Camera));
    camPtr->tkwin = new;
    camPtr->display = Tk_Display(new);

    camPtr->rwcamera = NULL;
    camPtr->scene = NULL;
    camPtr->cam_next = NULL;

    camPtr->widgetCmd = Tcl_CreateCommand(interp, Tk_PathName(camPtr->tkwin),
					  CameraWidgetCmd,
					  (ClientData) camPtr,
					  CameraCmdDeletedProc);
    camPtr->interp = interp;

    camPtr->backdrop_color = NULL;
    camPtr->backdrop_image = NULL;
    camPtr->backdrop_name = NULL;
    camPtr->max_height = CAMERA_MAX_VIEW_HEIGHT;
    camPtr->max_width = CAMERA_MAX_VIEW_WIDTH;
    camPtr->width = 0;
    camPtr->height = 0;
    camPtr->backdrop_width = 0;
    camPtr->backdrop_height = 0;
    camPtr->backdrop_x = 0;
    camPtr->backdrop_y = 0;
    camPtr->backdrop_offsetx = 0;
    camPtr->backdrop_offsety = 0;

    camPtr->cursor = None;
    camPtr->flags = 0;
    camPtr->scene = NULL;
    camPtr->highlightWidth = 0;
    camPtr->projection = NULL;
    camPtr->takeFocus = NULL;

    camPtr->state = tkNormalUid;
    camPtr->border = NULL;
    camPtr->borderWidth = 0;
    camPtr->relief = TK_RELIEF_FLAT;
    camPtr->highlightWidth = 0;
    camPtr->highlightBgColorPtr = NULL;
    camPtr->highlightColorPtr = NULL;

    Tk_SetClass(new, "Camera");
    Tk_CreateEventHandler(camPtr->tkwin,
			  ExposureMask|StructureNotifyMask|FocusChangeMask,
			  CameraEventProc, (ClientData) camPtr);

    
    rwcamera = RwCreateCamera(CAMERA_MAX_VIEW_WIDTH,
			      CAMERA_MAX_VIEW_HEIGHT, NULL);
    if (rwcamera == NULL) {
	sprintf(interp->result, "Renderware error: unable to create camera");
	Tk_DestroyWindow(camPtr->tkwin);
	return TCL_ERROR;
    }
    camPtr->rwcamera = rwcamera;

    if (ConfigureCamera(interp, camPtr, argc-2, argv+2, 0) != TCL_OK) {
	Tk_DestroyWindow(camPtr->tkwin);
	return TCL_ERROR;
    }

    Tk_SetWindowColormap(camPtr->tkwin, rw_params.cm_return);

    interp->result = Tk_PathName(camPtr->tkwin);
    return TCL_OK;
}


static void DestroyCamera(camPtr)
Camera* camPtr;
{
    if (camPtr->backdrop_image != NULL)
	RwDestroyRaster(camPtr->backdrop_image);

    Tk_FreeOptions(configSpecs, (char *) camPtr, camPtr->display, 0);

    RemoveSceneCamera(camPtr->scene, camPtr);

    if (camPtr->rwcamera != NULL)
	RwDestroyCamera(camPtr->rwcamera);

    Tcl_EventuallyFree((ClientData)camPtr, TCL_DYNAMIC);
}


static int ConfigureCamera(interp, camPtr, argc, argv, flags)
    Tcl_Interp *interp;		/* Used for error reporting. */
    register Camera *camPtr;	/* Information about widget;  may or may
				 * not already have values for some fields. */
    int argc;			/* Number of valid entries in argv. */
    char **argv;		/* Arguments. */
    int flags;			/* Flags to pass to Tk_ConfigureWidget. */
{
    RwCamera* camera = camPtr->rwcamera;
    RwRaster* raster;
    int bdWidth;
    int width;
    int height;

    if (Tk_ConfigureWidget(interp, camPtr->tkwin, configSpecs,
			   argc, argv, (char *) camPtr, flags) != TCL_OK) {
	return TCL_ERROR;
    }

    if ((camPtr->state != tkNormalUid) && (camPtr->state != tkDisabledUid)) {
	Tcl_AppendResult(interp, "bad state value \"", camPtr->state,
			 "\": must be normal or disabled", (char *) NULL);
	camPtr->state = tkNormalUid;
	return TCL_ERROR;
    }

    if (camPtr->border != NULL)
	Tk_SetBackgroundFromBorder(camPtr->tkwin, camPtr->border);

    if (camPtr->highlightWidth < 0)
	camPtr->highlightWidth = 0;

    bdWidth = camPtr->borderWidth + camPtr->highlightWidth;

    Tk_SetInternalBorder(camPtr->tkwin, bdWidth);

    /* Fix backdrop color */

    if (camPtr->backdrop_color != NULL) {
	RwRGBColor color;

	XColor2RwColor(camPtr->backdrop_color, &color);
	RwSetCameraBackColorStruct(camera, &color);

    }

    /* Fix backdrop image */
    if (camPtr->backdrop_name != NULL) {
	if ((raster = LoadRaster(interp, camPtr->backdrop_name)) == NULL) {
	    ckfree(camPtr->backdrop_name);
	    camPtr->backdrop_name = NULL;
	    return TCL_ERROR;
	}
    } else
	raster = NULL;

    /* Remove old image */
    if (camPtr->backdrop_image != NULL) {
	RwDestroyRaster(camPtr->backdrop_image);
	RwSetCameraBackdrop(camera, NULL);
    }

    camPtr->backdrop_image = raster;

    if (camPtr->backdrop_image != NULL) {
	RwSetCameraBackdrop(camera, camPtr->backdrop_image);
	RwSetCameraBackdropOffset(camera, 0, 0);
    }

    /* Fix height and width */

    if (camPtr->width < 0)
	camPtr->width = 0;
    else if (camPtr->width > CAMERA_MAX_VIEW_WIDTH)
	camPtr->width = CAMERA_MAX_VIEW_WIDTH;

    if (camPtr->height < 0)
	camPtr->width = 0;
    else if (camPtr->height > CAMERA_MAX_VIEW_HEIGHT) 
	camPtr->height = CAMERA_MAX_VIEW_HEIGHT;

    RwSetCameraViewport(camera,
			bdWidth, bdWidth,
			camPtr->width - 2*bdWidth,
			camPtr->height - 2*bdWidth);

    if (camPtr->backdrop_image != NULL) {
	RwSetCameraBackdropViewportRect(camera,
					bdWidth, bdWidth,
					camPtr->width - 2*bdWidth,
					camPtr->height - 2*bdWidth);
    }
    else
	RwSetCameraBackdropViewportRect(camera, bdWidth, bdWidth, 0, 0);


    RwInvalidateCameraViewport(camPtr->rwcamera);

    RwSetCameraViewOffset(camera,
			  camPtr->viewoffset.x,
			  camPtr->viewoffset.y);

    RwSetCameraViewwindow(camera,
			  camPtr->viewwindow.x,
			  camPtr->viewwindow.y);

    RwSetCameraNearClipping(camera,
			    FL2REAL(camPtr->nearclipping));

    if (strcmp(camPtr->projection, "parallel") == 0)
	RwSetCameraProjection(camera, rwPARALLEL);
    else if (strcmp(camPtr->projection, "perspective") == 0)
	RwSetCameraProjection(camera, rwPERSPECTIVE);
    else
	return TCL_ERROR;

    if ((camPtr->width > 0) || (camPtr->height > 0)) {
	Tk_GeometryRequest(camPtr->tkwin, camPtr->width,
			   camPtr->height);
    }

    if (Tk_IsMapped(camPtr->tkwin) && !(camPtr->flags & REDRAW_PENDING)) {
	Tcl_DoWhenIdle(DisplayCamera, (ClientData) camPtr);
	camPtr->flags |= REDRAW_PENDING;
    }
    return TCL_OK;
}


static void CameraCmdDeletedProc(clientData)
    ClientData clientData;	/* Pointer to widget record for widget. */
{
    Camera *camPtr = (Camera *) clientData;
    Tk_Window tkwin = camPtr->tkwin;

    /*
     * This procedure could be invoked either because the window was
     * destroyed and the command was then deleted (in which case tkwin
     * is NULL) or because the command was deleted, and then this procedure
     * destroys the widget.
     */

    if (tkwin != NULL) {
	camPtr->tkwin = NULL;
	Tk_DestroyWindow(tkwin);
    }
}

static Material* FindMaterial(rw)
RwMaterial* rw;
{
    Material* ptr = rw_material_list;

    if (rw == NULL)
	return NULL;

    while((ptr != NULL) && (ptr->rwmaterial != rw))
	ptr = ptr->material_next;
    return ptr;
}


static void AddMaterial(mp)
Material* mp;
{
    mp->material_next = rw_material_list;
    rw_material_list = mp;
}

static void RemoveMaterial(mp)
Material* mp;
{
    Material** ptr;

    ptr = &rw_material_list;
    while((*ptr != mp) && (*ptr != NULL))
	ptr = &(*ptr)->material_next;
    if (*ptr == NULL)
	return;
    *ptr = (*ptr)->material_next;
}



static Texture* FindTexture(rw)
RwTexture* rw;
{
    Texture* ptr = rw_texture_list;

    if (rw == NULL)
	return NULL;

    while((ptr != NULL) && (ptr->rwtexture != rw))
	ptr = ptr->texture_next;
    return ptr;
}

static void AddTexture(tp)
Texture* tp;
{
    tp->texture_next = rw_texture_list;
    rw_texture_list = tp;
}

static void RemoveTexture(tp)
Texture* tp;
{
    Texture** ptr;

    ptr = &rw_texture_list;
    while((*ptr != tp) && (*ptr != NULL))
	ptr = &(*ptr)->texture_next;
    if (*ptr == NULL)
	return;
    *ptr = (*ptr)->texture_next;
}

static void DereferenceTexture(tex)
RwTexture* tex;
{
    Texture* tp = (Texture*) RwGetTextureData(tex);

    if (tp == NULL)
	return;

    tp->refc--;
    if (tp->refc == 0) {
	RwRaster* ras;
	char* ras_name;

	RemoveTexture(tp);

	if ((ras = RwGetTextureRaster(tex)) != NULL) {
	    if ((ras_name = (char*) RwGetRasterData(ras)) != NULL) {
		fprintf(stderr, "Release Texture: %s\n", ras_name);
		ckfree(ras_name);
	    }
	}
	RwDestroyTexture(tex);
	ckfree(tp);
    }
}

static void ReferenceTexture(tex)
RwTexture* tex;
{
    Texture* tp = (Texture*) RwGetTextureData(tex);

    if (tp == NULL)
	return;

    if (!tp->deleted)
	tp->refc++;
}


/* Called when command is deleted */
static void DestroyTexture(tp)
Texture* tp;
{
    tp->deleted = 1;
    tp->command = NULL;
    DereferenceTexture(tp->rwtexture);
}

/*
** Unlink a camera from the scene camera list
*/
static void RemoveSceneCamera(scene, camera)
Scene* scene; Camera* camera;
{
    Camera** ptr;

    if (scene == NULL)
	return;

    ptr = &scene->cam_first;
    while((*ptr != camera) && (*ptr != NULL))
	ptr = &(*ptr)->cam_next;
    if (*ptr == NULL)
	return;
    *ptr = (*ptr)->cam_next;
    camera->scene = NULL;
}

/*
** AddSceneCamera:
**  Add the camera to the camera list for scene
**  This is normally invoked by the camera command render
*/
static void AddSceneCamera(scene, camera)
Scene* scene; Camera* camera;
{
    if (camera->scene == scene)
	return;
    RemoveSceneCamera(camera->scene, camera);

    camera->scene = scene;
    camera->cam_next = scene->cam_first;
    scene->cam_first = camera;
}

/*
** Remove model from scene top level model list
*/
static void RemoveSceneModel(scene, model)
Scene* scene; Model* model;
{
    Model** ptr = &scene->model_first;

    while((*ptr != model) && (*ptr != NULL))
	ptr = &(*ptr)->model_next;
    if (*ptr == NULL)
	return;
    *ptr = (*ptr)->model_next;
}

/*
** Invalidate all camera viewing a scene
*/
static void InvalidateScene(scene)
Scene* scene;
{
    Camera* camPtr = scene->cam_first;

    while(camPtr) {
	RwInvalidateCameraViewport(camPtr->rwcamera);
	camPtr = camPtr->cam_next;
    }
}


/*
** Render the scene in camera
*/
static void RenderSceneCamera(scene, camera)
Scene* scene; Camera* camera;
{
    RwCamera* rwcamera = camera->rwcamera;

    RwBeginCameraUpdate(rwcamera);
    RwClearCameraViewport(rwcamera);

    if (scene != NULL)
	RwRenderScene(scene->rwscene);

    RwEndCameraUpdate(rwcamera);
    RwShowCameraImage(rwcamera, (void *) Tk_WindowId(camera->tkwin));
    XSync(camera->display, False);
}

/*
** Render a camera view
*/
static void RenderCamera(camera)
Camera* camera;
{
    RenderSceneCamera(camera->scene, camera);
}

/*
** Render all cameras for a scene
*/
static void RenderScene(scene)
Scene* scene;
{
    Camera* camera = scene->cam_first;

    while(camera != NULL)  {
	RenderSceneCamera(scene, camera);
	camera = camera->cam_next;
    }
}


static int CameraWidgetCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Information about button widget. */
    Tcl_Interp* interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char** argv;		/* Argument strings. */
{
    register Camera* camPtr = (Camera*) clientData;
    int result = TCL_OK;
    size_t length;
    int c;

    if (argc < 2) {
	sprintf(interp->result,
		"wrong # args: should be \"%.50s option ?arg arg ...?\"",
		argv[0]);
	return TCL_ERROR;
    }

    Tcl_Preserve((ClientData) camPtr);
    c = argv[1][0];
    length = strlen(argv[1]);

    if ((c == 'c') && (strncmp(argv[1], "cget", length) == 0)
	&& (length >= 2)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " cget option\"",
		    (char *) NULL);
	    goto error;
	}
	result = Tk_ConfigureValue(interp, camPtr->tkwin, configSpecs,
				   (char *) camPtr, argv[2],
				   0);
    }
    else if ((c == 'c') && (strncmp(argv[1], "configure", length) == 0)
	       && (length >= 2)) {
	if (argc == 2) {
	    result = Tk_ConfigureInfo(interp, camPtr->tkwin, configSpecs,
				      (char *) camPtr, (char *) NULL, 
				      0);
	}
	else if (argc == 3) {
	    result = Tk_ConfigureInfo(interp, camPtr->tkwin, configSpecs,
				      (char *) camPtr, argv[2],
				      0);
	}
	else {
	    result = ConfigureCamera(interp, camPtr, argc-2, argv+2,
				     TK_CONFIG_ARGV_ONLY);
	}
    }
    else if ((c == 'r') && (strncmp(argv[1], "render", length) == 0)) {
	if (argc == 3) {
	    Scene* scene;

	    if (argv[2][0] == '\0') {
		RemoveSceneCamera(camPtr->scene, camPtr);
		goto update;
	    }
	    else if ((scene = FindScene(interp, argv[2])) != NULL) {
		AddSceneCamera(scene, camPtr);
		goto update;
	    }
	    else {
		Tcl_AppendResult(interp, "scene not found: should be \"",
				 argv[0], " render scene\"",
				 (char *) NULL);
		goto error;
	    }
	}
	else if (argc == 2) {
	    goto update;
	}
	else {    
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
			     argv[0], " render ?scene\"",
			     (char *) NULL);
	    goto error;
	}
    }
    else if ((c == 't') && (strncmp(argv[1], "tilt", length) == 0)) {
	if (argc == 3) {
	    double angle;

	    if (Tcl_GetDouble(interp, argv[2], &angle) == TCL_OK) {
		RwTiltCamera(camPtr->rwcamera, FL2REAL(angle));
		goto update;
	    }
	}
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " tilt angle\"",
			 (char *) NULL);
	goto error;	
    }
    else if ((c == 'p') && (strncmp(argv[1], "pan", length) == 0)) {
	if (argc == 3) {
	    double angle;

	    if (Tcl_GetDouble(interp, argv[2], &angle) == TCL_OK) {
		RwPanCamera(camPtr->rwcamera, FL2REAL(angle));
		goto update;
	    }
	}
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " pan angle\"",
			 (char *) NULL);
	goto error;	
    }
    else if ((c == 'r') && (strncmp(argv[1], "revolve", length) == 0)) {
	if (argc == 3) {
	    double angle;

	    if (Tcl_GetDouble(interp, argv[2], &angle) == TCL_OK) {
		RwRevolveCamera(camPtr->rwcamera, FL2REAL(angle));
		goto update;
	    }
	}
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " revolve angle\"",
			 (char *) NULL);
	goto error;
    }
    else if ((c == 'r') && (strncmp(argv[1], "rotate", length) == 0)) {
	double vx, vy, vz;
	if (argc == 5) {
	    RwReal m[4][4];
	    double sx, sy, sz;
	    RwV3d vector;

	    if (Scan3Double(interp, argv+2, &vx, &vy, &vz) != TCL_OK)
		return TCL_ERROR;
	    
	    RwGetCameraLookAt(camPtr->rwcamera, &vector);
	    sx = VectorNorm(REAL2FL(vector.x),
			    REAL2FL(vector.y), REAL2FL(vector.z));

	    RwGetCameraLookUp(camPtr->rwcamera, &vector);
	    sy = VectorNorm(REAL2FL(vector.x),
			    REAL2FL(vector.y), REAL2FL(vector.z));

	    RwGetCameraLookRight(camPtr->rwcamera, &vector);
	    sz = VectorNorm(REAL2FL(vector.x),
			    REAL2FL(vector.y), REAL2FL(vector.z));

	    RwScaleMatrix(rw_matrix1, sx, sy, sz, rwREPLACE);
	    RwRotateMatrix(rw_matrix1, 1.0, 0.0, 0.0, vx, rwPRECONCAT);
	    RwRotateMatrix(rw_matrix1, 0.0, 1.0, 0.0, vy, rwPRECONCAT);
	    RwRotateMatrix(rw_matrix1, 0.0, 0.0, 1.0, vz, rwPRECONCAT);
	    RwGetMatrixElements(rw_matrix1, m);

	    RwSetCameraLookAt(camPtr->rwcamera,
			      REAL2FL(m[0][0]),
			      REAL2FL(m[0][1]),
			      REAL2FL(m[0][2]));
	    RwSetCameraLookUp(camPtr->rwcamera,
			      REAL2FL(m[1][0]),
			      REAL2FL(m[1][1]),
			      REAL2FL(m[1][2]));
	    return TCL_OK;
	}
	else if (argc == 6) {
	    double angle;
	    if (Scan3Double(interp, argv+2, &vx, &vy, &vz) != TCL_OK)
		return TCL_ERROR;
	    if (Tcl_GetDouble(interp, argv[5], &angle) != TCL_OK)
		return TCL_ERROR;
	    RwRotateMatrix(rw_matrix1, vx, vy, vz, angle, rwREPLACE);
	    RwTransformCameraOrientation(camPtr->rwcamera, rw_matrix1);
	    return TCL_OK;
	}
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], "\" rotate Vx Vy Vz angle or ",
			 "rotate Ax Ay Az",
			 (char *) NULL);
	return TCL_ERROR;
    }
    else if ((c == 'm') && (strncmp(argv[1], "move", length) == 0)) {
	if (argc == 5) {
	    double dx, dy, dz;

	    if (Scan3Double(interp, argv+2, &dx, &dy, &dz) != TCL_OK)
		goto move_error;
	    RwVCMoveCamera(camPtr->rwcamera,
			   FL2REAL(dx), FL2REAL(dy), FL2REAL(dz));
	    goto update;
	}
    move_error:
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " move x y z\"",
			 (char *) NULL);
	goto error;
    }
    else {
	sprintf(interp->result,
		"bad option \"%.50s\": must be %s", argv[1],
		"cget, configure, render, tilt, pan, revolve or move");
	goto error;
    }

 ok:
    Tcl_Release((ClientData) camPtr);
    return result;

 update:
    Tcl_Release((ClientData) camPtr);
    RenderCamera(camPtr);
    return result;

 error:
    Tcl_Release((ClientData) camPtr);
    return TCL_ERROR;    
}

static void CameraEventProc(clientData, eventPtr)
    ClientData clientData;	/* Information about window. */
    XEvent *eventPtr;		/* Information about event. */
{
    register Camera* camPtr = (Camera*) clientData;

    if (eventPtr->type == Expose) {
	RwDamageCameraViewport(camPtr->rwcamera,
			       eventPtr->xexpose.x,
			       eventPtr->xexpose.y,
			       eventPtr->xexpose.width,
			       eventPtr->xexpose.height);
	if (eventPtr->xexpose.count == 0)
	    goto redraw;
    }
    else if (eventPtr->type == ConfigureNotify) {
	/*
	 * Must redraw after size changes, since layout could have changed
	 * and borders will need to be redrawn.
	 */
	goto redraw;
    } else if (eventPtr->type == DestroyNotify) {
	if (camPtr->tkwin != NULL) {
	    camPtr->tkwin = NULL;
	    Tcl_DeleteCommand(camPtr->interp,
		    Tcl_GetCommandName(camPtr->interp, camPtr->widgetCmd));
	}
	if (camPtr->flags & REDRAW_PENDING) {
	    Tcl_CancelIdleCall(DisplayCamera, (ClientData) camPtr);
	}
	DestroyCamera(camPtr);
    } else if (eventPtr->type == FocusIn) {
	if (eventPtr->xfocus.detail != NotifyInferior) {
	    camPtr->flags |= GOT_FOCUS;
	    if (camPtr->highlightWidth > 0) {
		goto redraw;
	    }
	}
    } else if (eventPtr->type == FocusOut) {
	if (eventPtr->xfocus.detail != NotifyInferior) {
	    camPtr->flags &= ~GOT_FOCUS;
	    if (camPtr->highlightWidth > 0) {
		goto redraw;
	    }
	}
    }
    return;

 redraw:
    if ((camPtr->tkwin != NULL) && !(camPtr->flags & REDRAW_PENDING)) {
	Tcl_DoWhenIdle(DisplayCamera, (ClientData) camPtr);
	camPtr->flags |= REDRAW_PENDING;
    }
}



static void DisplayCamera(clientData)
ClientData clientData;
{
    register Camera* camPtr = (Camera*) clientData;
    register Tk_Window tkwin = camPtr->tkwin;
    GC gc;

    camPtr->flags &= ~REDRAW_PENDING;

    if ((tkwin == NULL) || !Tk_IsMapped(tkwin)) {
	return;
    }

    if (camPtr->border != NULL) {
	Tk_Fill3DRectangle(tkwin, Tk_WindowId(tkwin),
			   camPtr->border, camPtr->highlightWidth,
			   camPtr->highlightWidth,
			   Tk_Width(tkwin) - 2*camPtr->highlightWidth,
			   Tk_Height(tkwin) - 2*camPtr->highlightWidth,
			   camPtr->borderWidth, camPtr->relief);
    }
    if (camPtr->highlightWidth != 0) {
	if (camPtr->flags & GOT_FOCUS) {
	    gc = Tk_GCForColor(camPtr->highlightColorPtr,
			       Tk_WindowId(tkwin));
	} else {
	    gc = Tk_GCForColor(camPtr->highlightBgColorPtr,
			       Tk_WindowId(tkwin));
	}
	Tk_DrawFocusHighlight(tkwin, gc, camPtr->highlightWidth,
			      Tk_WindowId(tkwin));
    }
    RenderSceneCamera(camPtr->scene, camPtr);
}


/*
** Given a name
** Locate the scene a and return a pointer to it
*/
static Scene* FindScene(interp, name)
Tcl_Interp* interp; char* name;
{
    Tcl_CmdInfo info;

    if (!Tcl_GetCommandInfo(interp, name, &info))
	return NULL;
    if (info.proc == SceneCmd)
	return (Scene*) info.clientData;
    return NULL;
}

static Scene* CreateSceneFromPath(interp, name)
Tcl_Interp* interp; char* name;
{
    Scene* scene;
    RwScene* rwscene;
    Tcl_CmdInfo info;

    if ((name[0] != '.') || (strchr(name+1, '.') != NULL)) {
	Tcl_AppendResult(interp, "bad path name: \"", name,
			 "\" scene must be top level", (char*) NULL);
	return NULL;
    }

    if (Tcl_GetCommandInfo(interp, name, &info)) {
	Tcl_AppendResult(interp, "bad path name: \"", name,
			 "\" command exists", (char*) NULL);
	return NULL;	
    }

    scene = (Scene*) ckalloc(sizeof(Scene));
    scene->interp = interp;
    scene->cam_first = NULL;
    scene->model_first = NULL;

    if ((rwscene = RwCreateScene()) == NULL) {
	Tcl_AppendResult(interp, "could not create: \"", name,
			 "\" limit?", (char*) NULL);
	ckfree(scene);
	return NULL;
    }

    scene->command = Tcl_CreateCommand(interp, name, SceneCmd,
				       (ClientData) scene, SceneDeletedProc);
    scene->rwscene = rwscene;
    RwSetSceneData(rwscene, (void*) scene);

    sprintf(interp->result, "%s", name);
    return scene;
}

/*
** SceneCmd:
**
**  scene pathName ?option value..?
**
**  pathName children - List of models
**
**  pathName lights   - List of lights
**
**  pathName render   - Render the scene in all active cameras
**
*/


int Tk_SceneCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    int c;
    int length;
    Scene* scene;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " pathName ?options?\"", (char *) NULL);
	return TCL_ERROR;
    }
    if ((scene = CreateSceneFromPath(interp, argv[1])) == NULL)
	return TCL_ERROR;

    if (ConfigureScene(interp, scene, argc-2, argv+2) != TCL_OK) {
	Tcl_DeleteCommand(interp, argv[1]);
	return TCL_ERROR;
    }
    return TCL_OK;
}


int ConfigureScene(interp, scene, argc, argv)
    Tcl_Interp *interp; Scene* scene;
    int argc; char** argv;
{

    return TCL_OK;
}
		   
static RwLight* AddLightName(rwlight, arg)
RwLight* rwlight; void* arg;
{
    Tcl_Interp* interp = (Tcl_Interp*) arg;
    Light* light = (Light*) RwGetLightData(rwlight);
    char* name = Tcl_GetCommandName(interp, light->command);

    Tcl_AppendResult(interp, name, " ", (char*) NULL);
    return NULL;
}

		   
int SceneCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Scene* scene = (Scene*) clientData;
    size_t length;
    int c;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " pathName ?options?\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);

    if ((c == 'r') && (strncmp(argv[1], "render", length) == 0)) {
	RenderScene(scene);
    }
    else if ((c == 'c') && (strncmp(argv[1], "children", length) == 0)) {
	Model* ptr = scene->model_first;

	while(ptr != NULL) {
	    char* name = Tcl_GetCommandName(interp, ptr->command);
	    
	    Tcl_AppendResult(interp, name, " ", (char*) NULL);
	    ptr = ptr->model_next;
	}
    }
    else if ((c == 'l') && (strncmp(argv[1],"lights", length) == 0)) {
	RwForAllLightsInScenePointer(scene->rwscene, 
				     AddLightName, (void*) interp);
    }
    else {
	sprintf(interp->result,
		"bad option \"%.50s\": must be %s", argv[1],
		"children or lights");
	return TCL_ERROR;
    }
    return TCL_OK;
}


RwLight* DeleteSceneLight(rwlight)
    RwLight* rwlight;
{
    Light* light = (Light*) RwGetLightData(rwlight);

    DestroyLight(light);
    return NULL;
}


void SceneDeletedProc(clientData)
    ClientData clientData;
{
    Scene* scene = (Scene*) clientData;
    Camera* camPtr;
    Model* model;

    RwForAllLightsInScene(scene->rwscene, DeleteSceneLight);

    /* Destroy all models & light */
    /* Since all models and lights have commands 
       we need to destory them */

    /* Delete all top level models (DestroyModel will do the work) */
    model = scene->model_first;
    scene->model_first = NULL;  /* THIS PREVENTS SEARCH LOOP */
    while(model != NULL) {
	Model* next = model->model_next;

	char* name = Tcl_GetCommandName(model->interp, model->command);
	
	DestroyModel(model);
	model = next;
    }

    /* Clear camera's for scene */
    camPtr = scene->cam_first;

    while(camPtr != NULL) {
	Camera* next = camPtr->cam_next;

	RwInvalidateCameraViewport(camPtr->rwcamera);
	camPtr->scene = NULL;
	camPtr->cam_next = NULL;
	camPtr = next;
    }
    RwDestroyScene(scene->rwscene);

    ckfree(scene);
}

/*
** ModelCmd:
**   pathName .scene.c1.c2...cn
**
**   Model Options:
**              -hints [container, hs, editable]
**              -state [on,off]
**              -align [x,y,xyz,none]
**              -tag <integer>
**              -position {X Y Z}    %% in global space
**              -jposition {X Y Z}   %% in parent space
**
**   create vertex X Y Z [VertexOptions]  => Return v<n>
**
**   create polygon V1 V2 ... Vn  [PolyOptions] => Return p<n>
**
**   itemconfigure v<n> [VertexOptions]
**
**   itemconfigure p<n> [PolyOptions]
**
**   itemconfigure all option value ?option value ...
**
**   itemconfigure <n> option value ?option value ...
**
**   itemcget p<n> ?option
**
**   translate Tx Ty Tz
**
**   scale Sx Sy Sz
**
**   rotate Vx Vy Vz theta
**
**   jtranslate Tx Ty Tz          - translate joint
**
**   jrotate Vx Vy Vz theta       - rotate joint
**
**
**   texturize [ sphere | cubic | map ]
**
**   VertexOptions:
**              -normal X Y Z
**              -uv U V
**
**   PolyOptions:
**                -tag <integer>
**                -ambient <ka>
**                -specular <ks>
**                -diffuse <kd>
**                -opacity <opacity>
**                -color <color>
**                -geometrysampling <"cloud" | "wireframe" | "solid">
**                -lightsampling <"facet" | "vertex">
**                -material <name>
**                -texture <name>
**                -texturemode "lit foreshorten"
**                -normal
**
*/

typedef struct {
    RwClump* clump;
    int index;
} ClumpIndex;

RwPolygon3d* WrapSetPolygonTexture(poly, tex)
RwPolygon3d* poly; RwTexture* tex;
{
    RwTexture* old_tex;

    if ((old_tex = RwGetPolygonTexture(poly)) != NULL)
	DereferenceTexture(old_tex);
    if (tex != NULL)
	ReferenceTexture(tex);
    return RwSetPolygonTexture(poly, tex);
}

RwPolygon3d* WrapSetPolygonMaterial(poly, mat)
RwPolygon3d* poly; RwMaterial* mat;
{
    RwTexture* tex;

    if ((tex = RwGetPolygonTexture(poly)) != NULL)
	DereferenceTexture(tex);
    if ((tex = RwGetMaterialTexture(mat)) != NULL)
	ReferenceTexture(tex);
    return RwSetPolygonMaterial(poly, mat);
}


RwClump* WrapSetVertexNormal(clumpix, normal)
ClumpIndex* clumpix; RwV3d* normal;
{
    return RwSetClumpVertexNormal(clumpix->clump, clumpix->index, normal);
}

RwV3d* WrapGetVertexNormal(clumpix, normal)
ClumpIndex* clumpix; RwV3d* normal;
{
    return RwGetClumpVertexNormal(clumpix->clump, clumpix->index, normal);
}

RwClump* WrapSetVertexPosition(clumpix, position)
ClumpIndex* clumpix; RwV3d* position;
{
    return RwSetClumpVertex(clumpix->clump, clumpix->index, position);
}

RwV3d* WrapGetVertexPosition(clumpix, position)
ClumpIndex* clumpix; RwV3d* position;
{
    return RwGetClumpVertex(clumpix->clump, clumpix->index, position);
}

RwClump* WrapSetVertexUV(clumpix, uv)
ClumpIndex* clumpix; RwUV* uv;
{
    return RwSetClumpVertexUV(clumpix->clump, clumpix->index, uv->u, uv->v);
}

RwUV* WrapGetVertexUV(clumpix, uv)
ClumpIndex* clumpix; RwUV* uv;
{
    return RwGetClumpVertexUV(clumpix->clump, clumpix->index, uv);
}


RwV3d* GetModelJPosition(rwclump, position)
RwClump* rwclump; RwV3d* position;
{
    RwGetClumpJointMatrix(rwclump, rw_matrix1);
    position->x = position->y = position->z = CREAL(0.0);
    return RwTransformPoint(position, rw_matrix1);
}


RwV3d* SetModelJPosition(rwclump, position)
RwClump* rwclump; RwV3d* position;
{
    RwGetClumpJointMatrix(rwclump, rw_matrix1);
    RwSetMatrixElement(rw_matrix1, 3, 0, position->x);
    RwSetMatrixElement(rw_matrix1, 3, 1, position->y);
    RwSetMatrixElement(rw_matrix1, 3, 2, position->z);
    RwTransformClumpJoint(rwclump, rw_matrix1, rwREPLACE);
    return position;
}

/* Strange operation, but what a heck */
RwV3d* SetModelPosition(rwclump, position)
RwClump* rwclump; RwV3d* position;
{
    RwGetClumpLTM(rwclump, rw_matrix1);
    RwInvertMatrix(rw_matrix1, rw_matrix2);
    RwTransformPoint(position, rw_matrix2);
    return SetModelJPosition(rwclump, position);
}

static TkRwOption modelopts[] = {
    { INTEGER, "-tag",
	  ParseRwInt, FormatRwInt,
	  (TkRwSetProc) RwSetClumpTag,
	  (TkRwGetProc) RwGetClumpTag },
    { INTEGER, "-state",
	  ParseRwState, FormatRwState,
	  (TkRwSetProc) RwSetClumpState,
	  (TkRwGetProc) RwGetClumpState },
    { INTEGER, "-align",
	  ParseRwAlignAxis, FormatRwAlignAxis,
	  (TkRwSetProc) RwSetClumpAxisAlignment,
	  (TkRwGetProc) RwGetClumpAxisAlignment },
    { VECTOR3D, "-position", 
	  ParseRwVector3D, FormatRwVector3D,	  
	  (TkRwSetProc) SetModelPosition,
	  (TkRwGetProc) RwGetClumpOrigin },
    { VECTOR3D, "-jposition", 
	  ParseRwVector3D, FormatRwVector3D,	  
	  (TkRwSetProc) SetModelJPosition,
	  (TkRwGetProc) GetModelJPosition },
    { NONE, NULL, NULL, NULL, NULL }
};


static TkRwOption polyopts[] = {
    { INTEGER, "-tag",
	  ParseRwInt, FormatRwInt,
	  (TkRwSetProc) RwSetPolygonTag,
	  (TkRwGetProc) RwGetPolygonTag },
    { REAL, "-ambient",
	  ParseRwReal, FormatRwReal,
	  (TkRwSetProc) RwSetPolygonAmbient,
	  (TkRwGetProc) RwGetPolygonAmbient },
    { REAL, "-diffuse",
	  ParseRwReal, FormatRwReal,
	  (TkRwSetProc) RwSetPolygonDiffuse,
	  (TkRwGetProc) RwGetPolygonDiffuse },
    { REAL, "-specular",
	  ParseRwReal, FormatRwReal,
	  (TkRwSetProc) RwSetPolygonSpecular,
	  (TkRwGetProc) RwGetPolygonSpecular },
    { REAL, "-opacity",
	  ParseRwReal, FormatRwReal,
	  (TkRwSetProc) RwSetPolygonOpacity,
	  (TkRwGetProc) RwGetPolygonOpacity },
    { COLOR, "-color",
	  ParseRwColor, FormatRwColor,
	  (TkRwSetProc) RwSetPolygonColorStruct,
	  (TkRwGetProc) RwGetPolygonColor },
    { INTEGER, "-geometrysampling",
	  ParseRwGeometrySampling, FormatRwGeometrySampling,
	  (TkRwSetProc) RwSetPolygonGeometrySampling,
	  (TkRwGetProc) RwGetPolygonGeometrySampling },
    { INTEGER, "-lightsampling",
	  ParseRwLightSampling, FormatRwLightSampling,
	  (TkRwSetProc) RwSetPolygonLightSampling,
	  (TkRwGetProc) RwGetPolygonLightSampling },
    { POINTER, "-texture",
	  ParseRwTexture, FormatRwTexture,
	  (TkRwSetProc) WrapSetPolygonTexture,
	  (TkRwGetProc) RwGetPolygonTexture }, 
    { POINTER, "-texturemodes",
	  ParseRwTextureModes, FormatRwTextureModes,
	  (TkRwSetProc) RwSetPolygonTextureModes,
	  (TkRwGetProc) RwGetPolygonTextureModes }, 
    { POINTER, "-material",
	  ParseRwMaterial, FormatRwMaterial,
	  (TkRwSetProc) WrapSetPolygonMaterial,
	  (TkRwGetProc) RwGetPolygonMaterial },
    { VECTOR3D, "-normal",
	  ParseRwVector3D, FormatRwVector3D,
	  (TkRwSetProc) NULL,
	  (TkRwGetProc) RwGetPolygonNormal},
    { NONE, NULL, NULL, NULL, NULL }
};


static TkRwOption vertexopts[] = {
    { VECTOR3D, "-normal",
	  ParseRwVector3D, FormatRwVector3D,
	  (TkRwSetProc) WrapSetVertexNormal,
	  (TkRwGetProc) WrapGetVertexNormal},
    { UV, "-uv",
	  ParseRwUV, FormatRwUV,
	  (TkRwSetProc) WrapSetVertexUV,
	  (TkRwGetProc) WrapGetVertexUV},
    { VECTOR3D, "-position",
	  ParseRwVector3D, FormatRwVector3D,
	  (TkRwSetProc) WrapSetVertexPosition,
	  (TkRwGetProc) WrapGetVertexPosition },
    { NONE, NULL, NULL, NULL, NULL }
};

/*
** Create a model with name scene.c1.c2 ...
*/
static Model* CreateModelFromPath(interp, name)
Tcl_Interp* interp; char* name;
{
    char* p;
    Tcl_CmdInfo info;
    
    if (Tcl_GetCommandInfo(interp, name, &info)) {
	Tcl_AppendResult(interp, "command \"", name, 
			 "\" already exist", (char*) NULL);
	return NULL;
    }

    if ((p = strrchr(name, '.')) != NULL) {
	/* Get Parent */
	*p = '\0';
	if (Tcl_GetCommandInfo(interp, name, &info)) {
	    RwClump* rwclump;
	    Model* model;
	    Tcl_Command command;

	    model = (Model*) ckalloc(sizeof(Model));

	    if (info.proc == SceneCmd) {
		Scene* scene = (Scene*) info.clientData;

		rwclump = RwCreateClump(10, 10);
		RwAddClumpToScene(scene->rwscene, rwclump);
		model->model_next = scene->model_first;
		scene->model_first = model;
	    }
	    else if (info.proc == ModelCmd) {
		Model* parent = (Model*) info.clientData;

		rwclump = RwCreateClump(10, 10);
		RwAddChildToClump(parent->rwclump, rwclump);
		model->model_next = NULL;
	    }
	    else {
		Tcl_AppendResult(interp, "bad parent path name \"", name, "\"",
				 (char*) NULL);
		*p = '.';
		ckfree(model);
		return NULL;
	    }
	    *p = '.';

	    command = Tcl_CreateCommand(interp, name, ModelCmd,
					(ClientData) model,
					ModelDeletedProc);
	    model->rwclump = rwclump;
	    model->interp = interp;
	    model->command = command;
	    model->npoly = 0;
	    model->poly_first = NULL;
	    model->poly_last = NULL;

	    RwSetClumpData(rwclump, (void*) model);
	    return model;
	}
	Tcl_AppendResult(interp, "bad parent path name \"", name, "\"",
			 (char*) NULL);
	*p = '.';
	return NULL;
    }
    Tcl_AppendResult(interp, "bad model path name \"", name, "\"",
		     (char*) NULL);
    return NULL;

}


int Tk_ModelCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Model* model;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " pathName ?options?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if ((model = CreateModelFromPath(interp, argv[1])) == NULL)
	return TCL_ERROR;

    if ((TkRwConfigure(interp, model->rwclump, modelopts,
		       argc-2, argv+2)) != TCL_OK) {
	Tcl_DeleteCommand(interp, argv[1]);
	return TCL_ERROR;
    }
    Tcl_AppendResult(interp, argv[1], (char*) NULL);
    return TCL_OK;
}


static int VertexCmd(model, interp, argc, argv)
Model* model; Tcl_Interp* interp; int argc; char** argv;
{
    if (argc >= 3) {
	double x;
	double y;
	double z;
	int ix;
	ClumpIndex clumpix;

	if (Tcl_GetDouble(interp, argv[0], &x) != TCL_OK)
	    return TCL_ERROR;
	if (Tcl_GetDouble(interp, argv[1], &y) != TCL_OK)
	    return TCL_ERROR;
	if (Tcl_GetDouble(interp, argv[2], &z) != TCL_OK)
	    return TCL_ERROR;

	ix = RwAddVertexToClump(model->rwclump, 
				FL2REAL(x), FL2REAL(y), FL2REAL(z));
	clumpix.clump = model->rwclump;
	clumpix.index = ix;

	if (TkRwConfigure(interp, &clumpix, 
			  vertexopts, argc-3, argv+3) == TCL_OK) {
	    sprintf(interp->result, "v%d", ix);
	    return TCL_OK;
	}
	else
	    return TCL_ERROR;
    }
    sprintf(interp->result,
	   "wrong # args: should be create vertex <x> <y> <z> ?options?");
    return TCL_ERROR;
}

#define FIXED_VERTICES 10

static RwPolygon3d* GetIndexedPolygon(interp, model, ix)
Tcl_Interp* interp; Model* model; int ix;
{
    RwPolygon3d* ptr;

    if (ix < 1 || ix > RwGetClumpNumPolygons(model->rwclump)) {
	sprintf(interp->result, "bad polygon index %d", ix);
	return NULL;
    }

    ptr = model->poly_first;
    ix--;
    while(ix--) {
	ptr = (RwPolygon3d*) RwGetPolygonData(ptr);
    }
    return ptr;
}


static int PolygonCmd(model, interp, argc, argv)
Model* model; Tcl_Interp* interp; int argc; char** argv;
{
    if (argc >= 3) {
	int v[FIXED_VERTICES];
	int* vp;
	int i;
	RwPolygon3d* poly;
	int result;

	if (argc > FIXED_VERTICES)
	    vp = (int*) ckalloc(argc*sizeof(int));
	else
	    vp = v;

	for (i = 0; i < argc; i++) {
	    if ((argv[i][0] != 'v') || !isdigit(argv[i][1]) ||
		Tcl_GetInt(interp, argv[i]+1, &vp[i]) != TCL_OK)
		break;
	}
	if (i < 3) {
	    if (vp != v)
		ckfree(vp);
	    goto badarg;
	}
	poly = RwAddPolygonToClump(model->rwclump, i, vp);
	result = TkRwConfigure(interp, poly, polyopts, argc-i, argv+i);
	if (result == TCL_OK) {
	    model->npoly++;

	    RwSetPolygonData(poly, NULL);
	    if (model->poly_last != NULL)
		RwSetPolygonData(model->poly_last, poly);
	    else
		model->poly_first = poly;
	    model->poly_last = poly;
	    sprintf(interp->result, "p%d", model->npoly);
	}
	if (vp != v)
	    ckfree(vp);
	return result;
    }

 badarg:
    sprintf(interp->result,
	    "wrong # args: should be create poly <v1> <v2> <v3> ... ?options?");
    return TCL_ERROR;
}


static int ModelCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc; char **argv;
{
    Model* model = (Model*) clientData;
    int result = TCL_OK;
    size_t length;
    int c;

    if (argc < 2) {
	sprintf(interp->result,
		"wrong # args: should be \"%.50s option ?arg arg ...?\"",
		argv[0]);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);

    if ((c == 'c') && (strncmp(argv[1], "cget", length) == 0)
	&& (length >= 2)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " cget option\"",
		    (char *) NULL);
	    return TCL_ERROR;
	}
	else {
	    TkRwOption* sp = TkRwFindSpec(interp, modelopts, argv[2]);
	    if (sp == NULL)
		return TCL_ERROR;
	    return TkRwGetSpec(interp, model->rwclump, sp, argv[2]);
	}
    }
    else if ((c == 'c') && (strncmp(argv[1], "configure", length) == 0)
	&& (length >= 2)) {
	if (argc == 2) {
	    return TkRwConfigureInfo(interp, model->rwclump,
				     modelopts, NULL);
	}
	else if (argc == 3) {
	    return TkRwConfigureInfo(interp, model->rwclump,
				     modelopts, argv[2]);
	}
	else {
	    return TkRwConfigure(interp, model->rwclump, 
				 modelopts, argc-2, argv+2);
	}
    }
    else if ((c == 'c') && (strncmp(argv[1], "create", length) == 0)) {
	if (argc < 6) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
			     argv[0], "\" create type arg1 arg2 arg3 ...",
			     (char *) NULL);
	    return TCL_ERROR;
	}
	c = argv[2][0];
	length = strlen(argv[2]);

	if ((c == 'v') && (strncmp(argv[2], "vertex", length) == 0)) {
	    return VertexCmd(model, interp, argc-3, argv+3);
	}
	else if ((c == 'p') && (strncmp(argv[2], "polygon", length) == 0)) {
	    return PolygonCmd(model, interp, argc-3, argv+3);
	}
	else {
	    sprintf(interp->result,
		    "create types: should be vertex or polygon");
	    return TCL_ERROR;
	}
    }
    if ((c == 'c') && (strncmp(argv[1], "children", length) == 0)) {
	RwClump* rwchild;

	/* Traverse all children */
	rwchild = RwGetFirstChildClump(model->rwclump);
	while(rwchild != NULL) {
	    Model* ptr = (Model*) RwGetClumpData(rwchild);
	    char* name = Tcl_GetCommandName(interp, ptr->command);

	    Tcl_AppendResult(interp, name, " ", (char*) NULL);

	    rwchild = RwGetNextClump(rwchild);
	}
	return TCL_OK;
    }
    else if ((c == 'p') && (strncmp(argv[1], "parent", length) == 0)) {
	RwClump* rwparent;
	RwScene* rwscene;
	char* name;

	if ((rwparent = RwGetClumpParent(model->rwclump)) != NULL) {
	    Model* ptr = (Model*) RwGetClumpData(rwparent); 
	    name = Tcl_GetCommandName(interp, ptr->command);
	}
	else if ((rwscene = RwGetClumpOwner(model->rwclump)) != NULL) {
	    Scene* ptr = (Scene*) RwGetSceneData(rwscene);
	    name = Tcl_GetCommandName(interp, ptr->command);
	}
	else
	    name = "";
	Tcl_AppendResult(interp, name, (char*) NULL);
	return TCL_OK;
    }
    else if ((c == 's') && (strncmp(argv[1], "scene", length) == 0)) {
	RwScene* rwscene;
	char* name;

	if ((rwscene = RwGetClumpOwner(model->rwclump)) != NULL) {
	    Scene* ptr = (Scene*) RwGetSceneData(rwscene);
	    name = Tcl_GetCommandName(interp, ptr->command);
	}
	else
	    name = "";
	Tcl_AppendResult(interp, name, (char*) NULL);
	return TCL_OK;
    }
    else if ((c == 'i') && (strncmp(argv[1], "itemconfigure", length) == 0) &&
	     (length >= 6)) {
	int index;
	int allflag;
	RwPolygon3d* ptr;

	if (argc < 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
			     argv[0], "\" itemconfigure tag ...",
			     (char *) NULL);
	    return TCL_ERROR;
	}
	c = argv[2][0];
	if (c == 'p') {
	    if (Tcl_GetInt(interp, argv[2]+1, &index) != TCL_OK)
		return TCL_ERROR;
	    if ((ptr = GetIndexedPolygon(interp, model, index)) == NULL)
		return TCL_ERROR;
	    if (argc == 3)
		return TkRwConfigureInfo(interp, ptr, polyopts, NULL);
	    else if (argc == 4) {
		if (strcmp(argv[3], "reverse") == 0) {
		    RwReversePolygonFace(ptr);
		    return TCL_OK;
		}
		return TkRwConfigureInfo(interp, ptr, polyopts, argv[3]);
	    }
	    else
		return TkRwConfigure(interp, ptr, polyopts, argc-3, argv+3);
	}
	else if (c == 'v') {
	    ClumpIndex clumpix;

	    if (Tcl_GetInt(interp, argv[2]+1, &index) != TCL_OK)
		return TCL_ERROR;
	    if ((index<1) || (index > RwGetClumpNumVertices(model->rwclump))) {
		sprintf(interp->result, "bad vertex index %d", index);
		return TCL_ERROR;
	    }
	    clumpix.clump = model->rwclump;
	    clumpix.index = index;

	    if (argc == 3)
		return TkRwConfigureInfo(interp,&clumpix, vertexopts, NULL);
	    else if (argc == 4)
		return TkRwConfigureInfo(interp,&clumpix,vertexopts,argv[3]);
	    else
		return TkRwConfigure(interp,&clumpix,vertexopts,argc-3,argv+3);
	}
	if ((argc > 4) && (strcmp(argv[2], "all") == 0))
	    allflag = 1;
	else if ((argc > 4) && isdigit(c)) {
	    allflag = 0;
	    if (Tcl_GetInt(interp, argv[2], &index) != TCL_OK)
		return TCL_ERROR;
	}
	else {
	    Tcl_AppendResult(interp, 
			     "item tag must be all, p<n>, v<n> or tagid ",
			     "got : \"", argv[2], "\"", (char*) NULL);
	    return TCL_ERROR;
	}

	ptr = model->poly_first;
	while(ptr != NULL) {
	    if (allflag || (RwGetPolygonTag(ptr) == index)) {
		if (TkRwConfigure(interp,ptr,polyopts,argc-3,argv+3) != TCL_OK)
		    return TCL_ERROR;
	    }
	    ptr = (RwPolygon3d*) RwGetPolygonData(ptr);
	}
	return TCL_OK;
    }
    else if ((c == 'i') && (strncmp(argv[1], "itemcget", length) == 0) &&
	     (length >= 6)) {
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
			     argv[0], " itemcget p<i> option\"",
			     (char *) NULL);
	    return TCL_ERROR;
	}
	else if (argv[2][0] == 'p') {
	    int index;
	    RwPolygon3d* ptr;	    
	    TkRwOption* sp;

	    if (Tcl_GetInt(interp, argv[2]+1, &index) != TCL_OK)
		return TCL_ERROR;
	    if ((ptr = GetIndexedPolygon(interp, model, index)) == NULL)
		return TCL_ERROR;
	    if ((sp = TkRwFindSpec(interp, polyopts, argv[3])) == NULL)
		return TCL_ERROR;
	    return TkRwGetSpec(interp, ptr, sp, argv[3]);
	}
	else if (argv[2][0] == 'v') {
	    int index;
	    ClumpIndex clumpix;
	    TkRwOption* sp;

	    if (Tcl_GetInt(interp, argv[2]+1, &index) != TCL_OK)
		return TCL_ERROR;
	    if ((index<1) || (index > RwGetClumpNumVertices(model->rwclump))) {
		sprintf(interp->result, "bad vertex index %d", index);
		return TCL_ERROR;
	    }
	    clumpix.clump = model->rwclump;
	    clumpix.index = index;
	    if ((sp = TkRwFindSpec(interp, vertexopts, argv[3])) == NULL)
		return TCL_ERROR;
	    return TkRwGetSpec(interp, &clumpix, sp, argv[3]);
	}
	else {
	    Tcl_AppendResult(interp, 
			     "item tag must be all p<n> or v<n> ",
			     "got : \"", argv[3], "\"", (char*) NULL);
	    return TCL_ERROR;
	}
    }
    else if ((c == 'j') && (strncmp(argv[1], "jtranslate", length) == 0) &&
	     (length >= 2)) {
	if (argc == 5) {
	    double dx, dy, dz;

	    if (Scan3Double(interp, argv+2, &dx, &dy, &dz) == TCL_OK) {
		RwTranslateMatrix(rw_matrix1, dx, dy, dz, rwREPLACE);
		RwTransformClumpJoint(model->rwclump, rw_matrix1, rwPRECONCAT);
		return TCL_OK;
	    }
	    return TCL_ERROR;
	}
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " move dx dy dz\"",
			 (char *) NULL);
	return TCL_ERROR;
    }
    else if ((c == 'j') && (strncmp(argv[1], "jrotate", length) == 0) &&
	     (length >= 2)) {
	double vx, vy, vz;
	if (argc == 5) {
	    if (Scan3Double(interp, argv+2, &vx, &vy, &vz) != TCL_OK)
		return TCL_ERROR;
	    RwGetClumpJointMatrix(model->rwclump, rw_matrix1);
	    OrientMatrix(rw_matrix1, vx, vy, vz);
	    RwTransformClumpJoint(model->rwclump, rw_matrix1, rwREPLACE);
	    return TCL_OK;
	}
	else if (argc == 6) {
	    double angle;
	    if (Scan3Double(interp, argv+2, &vx, &vy, &vz) != TCL_OK)
		return TCL_ERROR;
	    if (Tcl_GetDouble(interp, argv[5], &angle) != TCL_OK)
		return TCL_ERROR;
	    RwRotateMatrix(rw_matrix1, vx, vy, vz, angle, rwREPLACE);
	    RwTransformClumpJoint(model->rwclump, rw_matrix1, rwPRECONCAT);
	    return TCL_OK;
	}
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], "\" jrotate Vx Vy Vz angle or ",
			 "jrotate Ax Ay Az",
			 (char *) NULL);
	return TCL_ERROR;
    }
    else if ((c == 't') && (strncmp(argv[1], "translate", length) == 0)) {
	if (argc == 5) {
	    double tx, ty, tz;

	    if (Scan3Double(interp, argv+2, &tx, &ty, &tz) == TCL_OK) {
		RwTranslateMatrix(rw_matrix1, tx, ty, tz, rwREPLACE);
		RwTransformClump(model->rwclump, rw_matrix1, rwPRECONCAT);
		return TCL_OK;
	    }
	    return TCL_ERROR;
	}
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " translate Tx Ty Tz\"",
			 (char *) NULL);
	return TCL_ERROR;
    }
    else if ((c == 'r') && (strncmp(argv[1], "rotate", length) == 0)) {
	double vx, vy, vz;
	if (argc == 5) {
	    if (Scan3Double(interp, argv+2, &vx, &vy, &vz) != TCL_OK)
		return TCL_ERROR;
	    RwGetClumpMatrix(model->rwclump, rw_matrix1);
	    OrientMatrix(rw_matrix1, vx, vy, vz);
	    RwTransformClump(model->rwclump, rw_matrix1, rwREPLACE);
	    return TCL_OK;
	}
	else if (argc == 6) {
	    double angle;
	    if (Scan3Double(interp, argv+2, &vx, &vy, &vz) != TCL_OK)
		return TCL_ERROR;
	    if (Tcl_GetDouble(interp, argv[5], &angle) != TCL_OK)
		return TCL_ERROR;
	    RwRotateMatrix(rw_matrix1, vx, vy, vz, angle, rwREPLACE);
	    RwTransformClump(model->rwclump, rw_matrix1, rwPRECONCAT);
	    return TCL_OK;
	}
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], "\" rotate Vx Vy Vz angle or ",
			 "rotate Ax Ay Az",
			 (char *) NULL);
	return TCL_ERROR;
    }
    else if ((c == 's') && (strncmp(argv[1], "scale", length) == 0)) {
	if (argc == 5) {
	    double sx, sy, sz;

	    if (Scan3Double(interp, argv+2, &sx, &sy, &sz) == TCL_OK) {
		RwScaleMatrix(rw_matrix1, sx, sy, sz, rwREPLACE);
		RwTransformClump(model->rwclump, rw_matrix1, rwPRECONCAT);
		return TCL_OK;
	    }
	    return TCL_ERROR;
	}
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " scale Sx Sy Sz\"",
			 (char *) NULL);
	return TCL_ERROR;
    }
    else if ((c == 't') && (strncmp(argv[1], "texturize", length) == 0)) {
	if (argc == 2)
	    RwSphericalTexturizeClump(model->rwclump);
	else if (argc == 3) {
	    if (strcmp(argv[2], "sphere") == 0)
		RwSphericalTexturizeClump(model->rwclump);
	    else if (strcmp(argv[2], "cubic") == 0)
		RwCubicTexturizeClump(model->rwclump);
	    else if (strcmp(argv[2], "map") == 0)
		RwEnvMapClump(model->rwclump);
	    else {
		Tcl_AppendResult(interp, "bad arg: should be \"",
				 argv[0], " texturize sphere, cubic or map\"",
				 (char *) NULL);
		return TCL_ERROR;
	    }
	}
	else {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
			     argv[0], " texturize ?sphere, cubic or map?\"",
			     (char *) NULL);
	    return TCL_ERROR;
	}
	return TCL_OK;
    }
    else {
	sprintf(interp->result,
		"bad option \"%.50s\": must be %s", argv[1],
		"cget, configure, create, delete, itemcget, itemconfigure or "
		"move");
	return TCL_ERROR;
    }
}


static void DestroyModel(model)
Model* model;
{
    char* name = Tcl_GetCommandName(model->interp, model->command);

    Tcl_DeleteCommand(model->interp, name);
}


static RwPolygon3d* DereferencePolygonTexture(poly)
RwPolygon3d* poly;
{
    RwTexture* tex;

    if ((tex = RwGetPolygonTexture(poly)) != NULL)
	DereferenceTexture(tex);
    return poly;
}


static void ModelDeletedProc(clientData)
ClientData clientData;
{
    Model* model = (Model*) clientData;
    RwClump* rwchild;

    /* First destroy children */

    rwchild = RwGetFirstChildClump(model->rwclump);
    while(rwchild != NULL) {
	RwClump* rwnext = RwGetNextClump(rwchild);

	DestroyModel((Model*) RwGetClumpData(rwchild));
	rwchild = rwnext;
    }
    
    /* Dereference all textures */
    RwForAllPolygonsInClump(model->rwclump, DereferencePolygonTexture);

    /* Check if this is a toplevel model */
    if (RwGetClumpParent(model->rwclump) == NULL) {
	RwScene* rwscene = RwGetClumpOwner(model->rwclump);
	Scene* scene = (Scene*) RwGetSceneData(rwscene);

	RemoveSceneModel(scene, model);
    }
    RwDestroyClump(model->rwclump);
    ckfree((void*) model);
}


/*
** LightCmd:
**
**      pathName [conical,directional,point] ?options?
**
**      Options:
**         -brightness <lum>
**         -conangle  <theta>
**         -position {X Y Z}
**         -state <on | off>
**         -vector {X Y Z}
**
**      Read options
**         -type
*/

int WrapSetLightPosition(light, vector)
RwLight* light; RwV3d* vector;
{
    return (int) RwSetLightPosition(light, vector->x, vector->y, vector->z);
}

int WrapSetLightVector(light, vector)
RwLight* light; RwV3d* vector;
{
    return (int) RwSetLightVector(light, vector->x, vector->y, vector->z);
}


static TkRwOption lightopts[] = {
    { REAL, "-brightness", 
	  ParseRwReal, FormatRwReal,
	  (TkRwSetProc) RwSetLightBrightness,
	  (TkRwGetProc) RwGetLightBrightness },

    { REAL, "-conangle", 
	  ParseRwReal, FormatRwReal, 
	  (TkRwSetProc) RwSetLightConeAngle,
	  (TkRwGetProc) RwGetLightConeAngle },

    { INTEGER, "-state",
	  ParseRwState, FormatRwState,
	  (TkRwSetProc) RwSetLightState,
	  (TkRwGetProc) RwGetLightState },

    { VECTOR3D, "-vector",
	  ParseRwVector3D, FormatRwVector3D,
	  (TkRwSetProc) WrapSetLightVector,
	  (TkRwGetProc) RwGetLightVector },

    { VECTOR3D, "-position",
	  ParseRwVector3D, FormatRwVector3D,
	  (TkRwSetProc) WrapSetLightPosition,
	  (TkRwGetProc) RwGetLightPosition },

    { NONE, NULL, NULL, NULL, NULL }
};


Light* CreateLightFromPath(interp, name, type)
Tcl_Interp* interp; char* name; char* type;
{
    char* p;
    Tcl_CmdInfo info;
    Tcl_Command command;
    int length = strlen(name);
    RwLightType light_type;
    Scene* scene;
    Light* light;
    RwLight* rwlight;

    if ((name[0] != '.') || ((p = strchr(name+1, '.')) == NULL) ||
	(strchr(p+1, '.') != NULL) || (length <= 4)) {
	Tcl_AppendResult(interp, "bad path name: \"", name,
			 "\" light must be a child of scene", (char*) NULL);
	return NULL;
    }

    if (Tcl_GetCommandInfo(interp, name, &info)) {
	Tcl_AppendResult(interp, "command \"", name, 
			 "\" already exist", (char*) NULL);
	return NULL;
    }
    *p = '\0';
    if (!Tcl_GetCommandInfo(interp, name, &info) ||
	info.proc != SceneCmd) {
	*p = '.';
	Tcl_AppendResult(interp, "bad path name: \"", name,
			 "\" light must be a child of scene", (char*) NULL);
	return NULL;
    }
    *p = '.';

    if (strcmp(type, "conical") == 0)
	light_type = rwCONICAL;
    else if (strcmp(type, "directional") == 0)
	light_type = rwDIRECTIONAL;
    else if (strcmp(type, "point") == 0)
	light_type = rwPOINT;
    else {
	Tcl_AppendResult(interp, "bad light type: \"", name,
			 "\" must be conical, directional or point",
			 (char*) NULL);
	return NULL;
    }

    light = (Light*) ckalloc(sizeof(Light));
    scene = (Scene*) info.clientData;
    rwlight = RwCreateLight(light_type, 
			    CREAL(0.0), CREAL(0.0), CREAL(0.0),
			    CREAL(0.0));

    RwAddLightToScene(scene->rwscene, rwlight);

    command = Tcl_CreateCommand(interp, name, LightCmd,
				(ClientData) light,
				LightDeletedProc);
    light->rwlight = rwlight;
    light->command = command;
    light->interp = interp;
    RwSetLightData(rwlight, light);
    return light;
}


int Tk_LightCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Light* light;

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " pathName type ?options?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if ((light = CreateLightFromPath(interp, argv[1], argv[2])) == NULL)
	return TCL_ERROR;

    if ((TkRwConfigure(interp, light->rwlight, lightopts,
		       argc-3, argv+3)) != TCL_OK) {
	Tcl_DeleteCommand(interp, argv[1]);
	return TCL_ERROR;
    }
    Tcl_AppendResult(interp, argv[1], (char*) NULL);
    return TCL_OK;
}


static int LightCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc; char **argv;
{
    Light* light = (Light*) clientData;
    size_t length;
    int c;

    if (argc < 2) {
	sprintf(interp->result,
		"wrong # args: should be \"%.50s option ?arg arg ...?\"",
		argv[0]);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);

    if ((c == 'c') && (strncmp(argv[1], "cget", length) == 0)
	&& (length >= 2)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " cget option\"",
		    (char *) NULL);
	    return TCL_ERROR;
	}
	else {
	    TkRwOption* sp = TkRwFindSpec(interp, lightopts, argv[2]);
	    if (sp == NULL)
		return TCL_ERROR;
	    return TkRwGetSpec(interp, light->rwlight, sp, argv[2]);
	}
    }
    else if ((c == 'c') && (strncmp(argv[1], "configure", length) == 0)
	&& (length >= 2)) {
	if (argc == 2) {
	    return TkRwConfigureInfo(interp, light->rwlight,
				     lightopts, NULL);
	}
	else if (argc == 3) {
	    return TkRwConfigureInfo(interp, light->rwlight,
				     lightopts, argv[2]);
	}
	else {
	    return TkRwConfigure(interp, light->rwlight, 
				 lightopts, argc-2, argv+2);
	}
    }
    else {
	sprintf(interp->result,
		"bad option \"%.50s\": must be %s", argv[1],
		"cget or configure");
	return TCL_ERROR;
    }
    return TCL_OK;
}

static void DestroyLight(light)
Light* light;
{
    char* name = Tcl_GetCommandName(light->interp, light->command);

    Tcl_DeleteCommand(light->interp, name);
}

static void LightDeletedProc(clientData)
ClientData clientData;
{
    Light* light = (Light*) clientData;
    RwScene* rwscene = RwGetLightOwner(light->rwlight);
    Scene* scene = (Scene*) RwGetSceneData(rwscene);

    if (scene != NULL)
	InvalidateScene(scene);

    RwDestroyLight(light->rwlight);
    ckfree((void*) light);
}

/****************************************************************************

  TEXTURE COMMANDS

****************************************************************************/

RwTexture* WrapSetTextureRaster(tex, ras)
RwTexture* tex; RwRaster* ras;
{
    RwRaster* old_ras;
    char* name;

    if ((old_ras = RwGetTextureRaster(tex)) != NULL) {
	if ((name = (char*) RwGetRasterData(old_ras)) != NULL)
	    ckfree(name);
    }
    return RwSetTextureRaster(tex, ras);
}


static TkRwOption textureopts[] = {
    { POINTER, "-file",
	  ParseRwRasterFile, FormatRwRasterFile,
	  (TkRwSetProc) WrapSetTextureRaster,
	  (TkRwGetProc) RwGetTextureRaster },
    { NONE, NULL, NULL, NULL, NULL }
};


static Texture* CreateTextureFromPath(interp, name)
Tcl_Interp* interp; char* name;
{
    char* p;
    Tcl_CmdInfo info;
    RwTexture* tex;
    RwRaster* ras;
    Texture* tp;
    Tcl_Command command;

    if (Tcl_GetCommandInfo(interp, name, &info)) {
	Tcl_AppendResult(interp, "command \"", name, 
			 "\" already exist", (char*) NULL);
	return NULL;
    }

    tp = (Texture*) ckalloc(sizeof(Texture));

    if ((ras = RwCreateRaster(128, 128)) == NULL) { /* MUST HAVE IT ? */
	sprintf(interp->result, "Renderware error: unable to create raster");
	ckfree(tp);
	return NULL;
    }
    RwSetRasterData(ras, (void*) NULL);

    if ((tex = RwCreateTexture(ras)) == NULL) {
	sprintf(interp->result, "Renderware error: unable to create texture");
	RwDestroyRaster(ras);
	ckfree(tp);
	return NULL;
    }

    tp->rwtexture = tex;
    tp->interp = interp;
    tp->refc = 1;   /* Referenced by command */
    tp->deleted = 0;
    tp->command = Tcl_CreateCommand(interp, name, TextureCmd,
				    (ClientData) tp,
				    TextureDeletedProc);
    tp->texture_next = NULL;
    RwSetTextureData(tex, (void*) tp);
    AddTexture(tp);
    return tp;
}


static void TextureDeletedProc(clientData)
ClientData clientData;
{
    Texture* tp = (Texture*) clientData;

    /* FIX Dispose Texture by reference count !!! */
    DestroyTexture(tp);
}


static int TextureCmd(clientData, interp, argc, argv)
ClientData clientData; Tcl_Interp *interp;
int argc; char **argv;
{
    Texture* tp = (Texture*) clientData;
    size_t length;
    int c;

    if (argc < 2) {
	sprintf(interp->result,
		"wrong # args: should be \"%.50s option ?arg arg ...?\"",
		argv[0]);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);

    if ((c == 'c') && (strncmp(argv[1], "cget", length) == 0)
	&& (length >= 2)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " cget option\"",
			     (char *) NULL);
	    return TCL_ERROR;
	}
	else {
	    TkRwOption* sp = TkRwFindSpec(interp, textureopts, argv[2]);
	    if (sp == NULL)
		return TCL_ERROR;
	    return TkRwGetSpec(interp, tp->rwtexture, sp, argv[2]);
	}
    }
    else if ((c == 'c') && (strncmp(argv[1], "configure", length) == 0)
	&& (length >= 2)) {
	if (argc == 2) {
	    return TkRwConfigureInfo(interp, tp->rwtexture,
				     textureopts, NULL);
	}
	else if (argc == 3) {
	    return TkRwConfigureInfo(interp, tp->rwtexture,
				     textureopts, argv[2]);
	}
	else {
	    return TkRwConfigure(interp, tp->rwtexture,
				 textureopts, argc-2, argv+2);
	}
    }
    else {
	sprintf(interp->result,
		"bad option \"%.50s\": must be %s", argv[1],
		"cget, configure");
	return TCL_ERROR;
    }
    return TCL_OK;
}




int Tk_TextureCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Texture* tp;

    if ((tp = CreateTextureFromPath(interp, argv[1])) == NULL)
	return TCL_ERROR;

    if ((TkRwConfigure(interp, tp->rwtexture, textureopts,
		       argc-2, argv+2)) != TCL_OK) {
	Tcl_DeleteCommand(interp, argv[1]);
	return TCL_ERROR;
    }
    Tcl_AppendResult(interp, argv[1], (char*) NULL);
    return TCL_OK;
}



/****************************************************************************

  TEXTURE COMMANDS

****************************************************************************/

RwMaterial* WrapSetMaterialTexture(mat, tex)
RwMaterial* mat; RwTexture* tex;
{
    RwTexture* old_tex;

    if ((old_tex = RwGetMaterialTexture(mat)) != NULL)
	DereferenceTexture(old_tex);
    if (tex != NULL)
	ReferenceTexture(tex);
    return RwSetMaterialTexture(mat, tex);
}

static TkRwOption materialopts[] = {
    { REAL, "-ambient",
	  ParseRwReal, FormatRwReal,
	  (TkRwSetProc) RwSetMaterialAmbient,
	  (TkRwGetProc) RwGetMaterialAmbient },
    { REAL, "-diffuse",
	  ParseRwReal, FormatRwReal,
	  (TkRwSetProc) RwSetMaterialDiffuse,
	  (TkRwGetProc) RwGetMaterialDiffuse },
    { REAL, "-specular",
	  ParseRwReal, FormatRwReal,
	  (TkRwSetProc) RwSetMaterialSpecular,
	  (TkRwGetProc) RwGetMaterialSpecular },
    { REAL, "-opacity",
	  ParseRwReal, FormatRwReal,
	  (TkRwSetProc) RwSetMaterialOpacity,
	  (TkRwGetProc) RwGetMaterialOpacity },
    { COLOR, "-color",
	  ParseRwColor, FormatRwColor,
	  (TkRwSetProc) RwSetMaterialColorStruct,
	  (TkRwGetProc) RwGetMaterialColor },
    { INTEGER, "-geometrysampling",
	  ParseRwGeometrySampling, FormatRwGeometrySampling,
	  (TkRwSetProc) RwSetMaterialGeometrySampling,
	  (TkRwGetProc) RwGetMaterialGeometrySampling },
    { INTEGER, "-lightsampling",
	  ParseRwLightSampling, FormatRwLightSampling,
	  (TkRwSetProc) RwSetMaterialLightSampling,
	  (TkRwGetProc) RwGetMaterialLightSampling },
    { POINTER, "-texture",
	  ParseRwTexture, FormatRwTexture,
	  (TkRwSetProc) WrapSetMaterialTexture,
	  (TkRwGetProc) RwGetMaterialTexture },	  
    { POINTER, "-texturemodes",
	  ParseRwTextureModes, FormatRwTextureModes,
	  (TkRwSetProc) RwSetMaterialTextureModes,
	  (TkRwGetProc) RwGetMaterialTextureModes }, 
    { NONE, NULL, NULL, NULL, NULL }
};


static Material* CreateMaterialFromPath(interp, name)
Tcl_Interp* interp; char* name;
{
    char* p;
    Tcl_CmdInfo info;
    RwMaterial* rwmaterial;
    Material* mp;
    Tcl_Command command;

    if (Tcl_GetCommandInfo(interp, name, &info)) {
	Tcl_AppendResult(interp, "command \"", name, 
			 "\" already exist", (char*) NULL);
	return NULL;
    }

    mp = (Material*) ckalloc(sizeof(Material));

    if ((rwmaterial = RwCreateMaterial()) == NULL) {
	sprintf(interp->result, "Renderware error: unable to create camera");
	ckfree(mp);
	return NULL;
    }
    mp->rwmaterial = rwmaterial;
    mp->interp = interp;
    mp->command = Tcl_CreateCommand(interp, name, MaterialCmd,
				    (ClientData) mp,
				    MaterialDeletedProc);
    mp->material_next = NULL;

    AddMaterial(mp);
    return mp;
}


static void MaterialDeletedProc(clientData)
ClientData clientData;
{
    Material* mp = (Material*) clientData;
    RwMaterial* mat;

    RemoveMaterial(mp);
    if ((mat = mp->rwmaterial) != NULL) {
	RwTexture* tex;

	if ((tex= RwGetMaterialTexture(mat)) != NULL)
	    DereferenceTexture(tex);
	RwDestroyMaterial(mat);
    }
    ckfree(mp);
}


static int MaterialCmd(clientData, interp, argc, argv)
ClientData clientData; Tcl_Interp *interp;
int argc; char **argv;
{
    Material* mp = (Material*) clientData;
    size_t length;
    int c;

    if (argc < 2) {
	sprintf(interp->result,
		"wrong # args: should be \"%.50s option ?arg arg ...?\"",
		argv[0]);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);

    if ((c == 'c') && (strncmp(argv[1], "cget", length) == 0)
	&& (length >= 2)) {
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
		    argv[0], " cget option\"",
			     (char *) NULL);
	    return TCL_ERROR;
	}
	else {
	    TkRwOption* sp = TkRwFindSpec(interp, materialopts, argv[2]);
	    if (sp == NULL)
		return TCL_ERROR;
	    return TkRwGetSpec(interp, mp->rwmaterial, sp, argv[2]);
	}
    }
    else if ((c == 'c') && (strncmp(argv[1], "configure", length) == 0)
	&& (length >= 2)) {
	if (argc == 2) {
	    return TkRwConfigureInfo(interp, mp->rwmaterial,
				     materialopts, NULL);
	}
	else if (argc == 3) {
	    return TkRwConfigureInfo(interp, mp->rwmaterial,
				     materialopts, argv[2]);
	}
	else {
	    return TkRwConfigure(interp, mp->rwmaterial,
				 materialopts, argc-2, argv+2);
	}
    }
    else {
	sprintf(interp->result,
		"bad option \"%.50s\": must be %s", argv[1],
		"cget, configure");
	return TCL_ERROR;
    }
    return TCL_OK;
}


int Tk_MaterialCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Material* mp;

    if ((mp = CreateMaterialFromPath(interp, argv[1])) == NULL)
	return TCL_ERROR;

    if ((TkRwConfigure(interp, mp->rwmaterial, materialopts,
		       argc-2, argv+2)) != TCL_OK) {
	Tcl_DeleteCommand(interp, argv[1]);
	return TCL_ERROR;
    }
    Tcl_AppendResult(interp, argv[1], (char*) NULL);
    return TCL_OK;
}


int Tk_RwDestroyCmd(clientData, interp, argc, argv)
    ClientData clientData;		/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Tk_Window window;
    Tk_Window tkwin = (Tk_Window) clientData;
    int i;

    for (i = 1; i < argc; i++) {
	if ((window = Tk_NameToWindow(interp, argv[i], tkwin)) != NULL)
	    Tk_DestroyWindow(window);
	else {
	    Tcl_CmdInfo info;

	    if (!Tcl_GetCommandInfo(interp, argv[i], &info))
		return TCL_ERROR;
	    if (info.deleteProc != NULL) {
		Tcl_DeleteCommand(interp, argv[i]);
		Tcl_ResetResult(interp);
	    }
	    else {
		sprintf(interp->result,
			"bad command name \"%s\"", argv[i]);
		return TCL_ERROR;
	    }
	}
    }
    return TCL_OK;
}


int Tk_RwInit(display, vis, cmap)
Display* display; Visual** vis; Colormap* cmap;
{
    XColorEntry* cp;

    rw_params.dpy = display;
    rw_params.screen = DefaultScreen(display);
#ifdef DEBUG
    rw_params.use_shm = False; 
#else
    rw_params.use_shm = True;
#endif
    if (!RwOpen("X11", &rw_params))
	return -1;
    *vis = rw_params.vis_return;
    *cmap = rw_params.cm_return;

    Tcl_InitHashTable(&rw_xcolors, TCL_STRING_KEYS);
    cp = xColors;
    while(cp->name != NULL) {
	int created;
	Tcl_HashEntry* h;

	h = Tcl_CreateHashEntry(&rw_xcolors, cp->name, &created);
	Tcl_SetHashValue(h, cp);
	cp++;
    }
    rw_material_list = NULL;
    rw_texture_list = NULL;
    /* Two scratch matrices */
    rw_matrix1 = RwCreateMatrix();
    rw_matrix2 = RwCreateMatrix();

#ifdef DEBUG
    fprintf(stderr, "bits_per_rgb = %d\n\r", 
	    rw_params.vis_return->bits_per_rgb);
    fprintf(stderr, "red_mask = %d, green_mask = %d, blue_mask= %d\n\r", 
	    rw_params.vis_return->red_mask,
	    rw_params.vis_return->green_mask,
	    rw_params.vis_return->blue_mask);
    fprintf(stderr, "map_entries = %d\n\r", 
	    rw_params.vis_return->map_entries);
#endif

    return (rw_params.use_shm == True);
}


int Tk_RwTerminate() 
{
    Tcl_DeleteHashTable(&rw_xcolors);
    RwClose();
}
