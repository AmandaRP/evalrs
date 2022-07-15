#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define SIGN(x) ((x) < 0 ? -1 : ((x) > 0 ? 1 : 0))

static int cmp(const void *a, const void *b)
{
	return !(a && b) ? 0 : strcmp(*(char *const *)a, *(char *const *)b);
}

/******************************************************************************
* d is an array of *dc names of elements.
* x, y are _sorted_ arrays of *xc, *yc (respectively) names of elements.
* xr, yr are arrays of *xc, *yc ranks of the corresponding elements of x, y.
* *p is the penalty.
* *norm indicates whether to normalize the result.
* *res is the result.
******************************************************************************/
void kbarp(char **d, int *dc, char **x, int *xr, int *xc,
	   char **y, int *yr, int *yc, double *p, int *norm, double *res)
{
	int i;
	*res = 0;
	#pragma omp parallel for reduction(+:res[:1])
	for (i = 0; i < *dc; i += 1) {
		char **xi, **yi;
		int xri, yri, j;
		xi = bsearch(d + i, x, *xc, sizeof *x, cmp);
		yi = bsearch(d + i, y, *yc, sizeof *y, cmp);
		xri = xi - x;	/* Ojo: `xi' may be NULL. */
		yri = yi - y;	/* Ojo: `yi' may be NULL. */
		for (j = 1 + i; j < *dc; j += 1) {
			char **xj, **yj;
			int xrj, yrj;
			xj = bsearch(d + j, x, *xc, sizeof *x, cmp);
			yj = bsearch(d + j, y, *yc, sizeof *y, cmp);
			xrj = xj - x;	/* Ojo: `xj' may be NULL. */
			yrj = yj - y;	/* Ojo: `yj' may be NULL. */
			/* case 1 */
			if (xi && yi && xj && yj)
				*res += SIGN(xr[xri] - xr[xrj])
					!= SIGN(yr[yri] - yr[yrj]);
			/* case 2 */
			else if (xi && yi && xj && !yj)
				*res += (xr[xri] - xr[xrj]) > 0;
			else if (xi && yi && !xj && yj)
				*res += (yr[yri] - yr[yrj]) > 0;
			else if (xi && !yi && xj && yj)
				*res += (xr[xri] - xr[xrj]) < 0;
			else if (!xi && yi && xj && yj)
				*res += (yr[yri] - yr[yrj]) < 0;
			/* case 3 */
			else if ((xi && !yi && !xj && yj)
				|| (!xi && yi && xj && !yj))
				*res += 1L;
			/* case 4 */
			else if ((xi && !yi && xj && !yj)
				|| (!xi && yi && !xj && yj))
				*res += *p;
			/* all other logical possibilities impossible here */
			else assert(0);
		}
	}
	if (*norm && *dc > 1)
		*res /= *dc * (*dc - 1) / 2;	/* division by *dc-choose-2 */
}

static R_NativePrimitiveArgType kbarp_type[] = {
	/* char **d, int *dc, char **x, int *xr, int *xc, */
	STRSXP,      INTSXP,  STRSXP,   INTSXP,  INTSXP,
	/* char **y, int *yr, int *yc, double *p, int *norm, double *res */
	STRSXP,      INTSXP,  INTSXP,  REALSXP,   INTSXP,    REALSXP
};
#ifndef ARRAY_LEN
#define ARRAY_LEN(x) (sizeof (x) / sizeof *(x))
#endif
static const R_CMethodDef cMethods[] = {
	{ "kbarp", (DL_FUNC)&kbarp, ARRAY_LEN(kbarp_type), kbarp_type },
	{       0,               0,                     0,          0 }
};

void R_init_evalrs(DllInfo *info)
{
	R_registerRoutines(info, cMethods, 0, 0, 0);
	R_useDynamicSymbols(info, FALSE);
	R_forceSymbols(info, TRUE);
}
