/* See LICENSE file for copyright and license details. */

/* appearance */
static unsigned int borderpx  = 1;        /* border pixel of windows */
static unsigned int snap      = 32;       /* snap pixel */
static int showbar            = 1;        /* 0 means no bar */
static int topbar             = 0;        /* 0 means bottom bar */
static const int focusonwheel = 0;
static char font[]            = "monospace:size=10";
static char dmenufont[]       = "monospace:size=10";
static const char *fonts[]    = { font };
static char normbgcolor[]     = "#222222";
static char normbordercolor[] = "#444444";
static char normfgcolor[]     = "#bbbbbb";
static char selfgcolor[]      = "#eeeeee";
static char selbordercolor[]  = "#005577";
static char selbgcolor[]      = "#005577";
static const int startwithgaps[] = { 0 };   /* 1 means gaps are used by default, this can be customized for each tag */
static const unsigned int gappx[] = { 10 }; /* default gap between windows in pixels, this can be customized for each tag */
static char *colors[][3]      = {
       /*               fg           bg           border */
       [SchemeNorm] = { normfgcolor, normbgcolor, normbordercolor },
       [SchemeSel]  = { selfgcolor,  selbgcolor,  selbordercolor  },
};
static const unsigned int baralpha    = 0xd0;
static const unsigned int borderalpha = OPAQUE;
static const unsigned int alphas[][3] = {
	/*               fg      bg        border */
	[SchemeNorm] = { OPAQUE, baralpha, borderalpha },
	[SchemeSel]  = { OPAQUE, baralpha, borderalpha },
};

/* tagging */
static const char *tags[] = { "~", "www", "doc", "fs", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class      instance    title       tags mask     isfloating   monitor */
	{ "Gimp",     NULL,       NULL,       0,            1,           -1 },
	{ "Firefox",  NULL,       NULL,       1 << 8,       0,           -1 },
	{ "st_float", NULL,       "st_float", 0,            1,           -1 },
};

/* layout(s) */
static float mfact              = 0.5; /* factor of master area size [0.05..0.95] */
static int nmaster              = 1;   /* number of clients in master area */
static int resizehints          = 1;   /* 1 means respect size hints in tiled resizals */
static const int lockfullscreen = 1;   /* 1 will force focus on the fullscreen window */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
 	//{ "TTT",      bstack },
 	//{ "===",      bstackhoriz },
	{ NULL,      NULL },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/*
 * Xresources preferences to load at startup
 */
ResourcePref resources[] = {
       { "font",               STRING,  &font },
       { "dmenufont",          STRING,  &dmenufont },
       { "normbgcolor",        STRING,  &normbgcolor },
       { "normbordercolor",    STRING,  &normbordercolor },
       { "normfgcolor",        STRING,  &normfgcolor },
       { "selbgcolor",         STRING,  &selbgcolor },
       { "selbordercolor",     STRING,  &selbordercolor },
       { "selfgcolor",         STRING,  &selfgcolor },
       { "borderpx",           INTEGER, &borderpx },
       { "snap",               INTEGER, &snap },
       { "showbar",            INTEGER, &showbar },
       { "topbar",             INTEGER, &topbar },
       { "nmaster",            INTEGER, &nmaster },
       { "resizehints",        INTEGER, &resizehints },
       { "mfact",              FLOAT,   &mfact },
};

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", normbgcolor, "-nf", normfgcolor, "-sb", selbordercolor, "-sf", selfgcolor, NULL };
static const char *termcmd[]  = { "st", NULL };
static const char *termcmdfloat[]  = { "st", "-c", "st_float", "-t", "st_float", NULL };

static const Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_d,      spawn,          {.v = dmenucmd } },
	{ MODKEY,                       XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY,                       XK_a,      spawn,          {.v = termcmdfloat } },
	//{ MODKEY|ShiftMask,             XK_a,      spawn,          {.v = termcmdfloat } },
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_k,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_l,      focusstack,     {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_j,      incnmaster,     {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_minus,  incnmaster,     {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_k,      rotatestack,    {.i = +1 } },
        { MODKEY|ShiftMask,             XK_l,      rotatestack,    {.i = -1 } },
	{ MODKEY,                       XK_j,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_minus,  setmfact,       {.f = +0.05} },
	{ MODKEY,                       XK_z,      zoom,           {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY,                       XK_q,      killclient,     {0} },
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
 	//{ MODKEY,                       XK_u,      setlayout,      {.v = &layouts[3]} },
 	//{ MODKEY,                       XK_o,      setlayout,      {.v = &layouts[4]} },
	{ MODKEY,                       XK_space,  setlayout,      {0} },
	{ MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
        { MODKEY,                       XK_s,      togglecanfocusfloating,{0} },
	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
	{ MODKEY|ControlMask,           XK_comma,  cyclelayout,    {.i = -1 } },
	{ MODKEY|ControlMask,           XK_period, cyclelayout,    {.i = +1 } },
	{ MODKEY,                       XK_F5,     xrdb,           {.v = NULL } },
	{ MODKEY,                       XK_i,      setgaps,        {.i = -5 } },
	{ MODKEY,                       XK_o,      setgaps,        {.i = +5 } },
	{ MODKEY|ShiftMask,             XK_i,      setgaps,        {.i = GAP_RESET } },
	{ MODKEY|ShiftMask,             XK_o,      setgaps,        {.i = GAP_TOGGLE } },
	{ MODKEY|ShiftMask,             XK_e,      quit,           {0} },
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static const Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

/* signal definitions */
/* signum must be greater than 0 */
/* trigger signals using `xsetroot -name "fsignal:<signum>"` */
static Signal signals[] = {
    /* signum   function    argument */
    { 1,        xrdb,       {.v = NULL} },
    { 2,        setgaps,    {.i = -5} },
    { 3,        setgaps,    {.i = +5} },
    { 4,        setgaps,    {.i = GAP_RESET} },
};
