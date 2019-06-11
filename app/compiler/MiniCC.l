%{
import Debug.Trace
%}
D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			([Ee][+-]?{D}+)
P           ([Pp][+-]?{D}+)
FS			(f|F|l|L)
IS          ((u|U)|(u|U)?(l|L|ll|LL)|(l|L|ll|LL)(u|U))
%%
auto			{ printf("AUTO"); }
case			{ printf("CASE"); }
char			{ printf("CHAR"); }
const			{ printf("CONST"); }
continue		{ printf("CONTINUE"); }
default 		{ printf("DEFAULT"); }
do			    { printf("DO"); }
double		    { printf("DOUBLE"); }
else			{ printf("ELSE"); }
enum			{ printf("ENUM"); }
extern 		    { printf("EXTERN"); }
float			{ printf("FLOAT"); }
for 			{ printf("FOR"); }
goto			{ printf("GOTO"); }
if  			{ printf("IF"); }
inline	    	{ printf("INLINE"); }
int			    { printf("INT"); }
long			{ printf("LONG"); }
register		{ printf("REGISTER"); }
restrict		{ printf("RESTRICT"); }
return		    { printf("RETURN"); }
short			{ printf("SHORT"); }
signed   		{ printf("SIGNED"); }
sizeof   		{ printf("SIZEOF"); }
static  		{ printf("STATIC"); }
struct 	    	{ printf("STRUCT"); }
switch  		{ printf("SWITCH"); }
typedef		    { printf("TYPEDEF");}
union			{ printf("UNION"); }
unsigned		{ printf("UNSIGNED"); }
void			{ printf("VOID"); }
volatile		{ printf("VOLATILE"); }
while			{ printf("WHILE"); }
>>=			{ printf("RIGHT_ASSIGN"); }
<<=			{ printf("LEFT_ASSIGN"); }
\+=			{ printf("ADD_ASSIGN"); }
-=			{ printf("SUB_ASSIGN"); }
\*=			{ printf("MUL_ASSIGN"); }
/=			{ printf("DIV_ASSIGN"); }
%=			{ printf("MOD_ASSIGN"); }
&=			{ printf("AND_ASSIGN"); }
^=			{ printf("XOR_ASSIGN");  }
\|=			{ printf("OR_ASSIGN");  }
>>			{ printf("RIGHT_OP");  }
<<			{ printf("LEFT_OP");  }
\+\+			{ printf("INC_OP");  }
--			{ printf("DEC_OP");  }
->			{ printf("PTR_OP");  }
&&			{ printf("AND_OP");  }
\|\|			{ printf("OR_OP");  }
<=			{ printf("LE_OP");  }
>=			{ printf("GE_OP");  }
==			{ printf("EQ_OP");  }
!=			{ printf("NE_OP");  }
;			{ printf(";");  }
\{		{ printf("{");  }
\}		{ printf("}");  }
,			{ printf(",");  }
:			{ printf(":");  }
=			{ printf("=");  }
\(			{ printf("(");  }
\)			{ printf(")");  }
\[		{ printf("[");  }
\]		{ printf("]");  }
\.			{ printf(".");  }
&			{ printf("&");  }
!			{ printf("!");  }
~			{ printf("~");  }
-			{ printf("-");  }
\+			{ printf("+");  }
\*			{ printf("*");  }
/			{ printf("/");  }
%			{ printf("%");  }
<			{ printf("<");  }
>			{ printf(">");  }
^			{ printf("^");  }
\|			{ printf("||");  }
\?			{ printf("?");  }
{L}({L}|{D})*		{ printf("IDENTIFIER"); }
0[xX]{H}+{IS}?		{ printf("CONSTANT");  }
0[0-7]*{IS}?		{ printf("CONSTANT");  }
[1-9]{D}*{IS}?		{ printf("CONSTANT");  }
{D}+{E}{FS}?		{ printf("CONSTANT");  }
{D}*\.{D}+{E}?{FS}?	{ printf("CONSTANT"); }
{D}+\.{D}*{E}?{FS}?	{ printf("CONSTANT"); }
0[xX]{H}+{P}{FS}?	{ printf("CONSTANT");  }
0[xX]{H}*\.{H}+{P}{FS}?		{ printf("CONSTANT");  }
0[xX]{H}+\.{H}*{P}{FS}?		{ printf("CONSTANT");  }

[ \t\v\n\f]		{ printf("SPACE"); }
.			{ printf("ERROR!"); }
%%
printf (str) = trace str $ return Nothing
