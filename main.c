
#include<iostream>
#include<string.h>
#include<math.h>
#include<time.h>
#include<stdlib.h>

const int R_Row = 5;
const int MAXN = 100;
const int MAX = MAXN*MAXN;

int TempR = 0;
float TempV_New = 0;
float TempV_Last = 0;
int N = R_Row;

struct Matrix_V {
	int n, m;
	float a[MAXN][MAXN];
	void Clear() {
		n = m = 0;
		memset(a, 0, sizeof(a));
	}
	int ones(int n2, int m2) {
		n = n2; m = m2;
		for (int i = 0; i<n; i++)
			for (int j = 0; j<m; j++)
				a[i][j] = 1;
		return 0;
	}
	int Zeros(int n2, int m2) {
		n = n2; m = m2;
		for (int i = 0; i<n; i++)
			for (int j = 0; j<m; j++)
				a[i][j] = 0;
		return 0;
	}
	Matrix_V sumcol() {
		Matrix_V Res;
		Res.Clear();
		Res.Zeros(1, m);
		for (int i = 0; i<n; i++)
			for (int j = 0; j<m; j++)
				Res.a[0][j] += a[i][j];
		return Res;
	}
	Matrix_V sumrow() {
		Matrix_V Res;
		Res.Clear();
		Res.Zeros(m, 1);
		for (int j = 0; j<m; j++)
			for (int i = 0; i<n; i++)
				Res.a[i][0] += a[i][j];
		return Res;
	}

};

struct Matrix {
	int n;
	float a[MAX];
	void Clear() {
		n = 0;
		memset(a, 0, sizeof(a));
	}
	int ones(int n2) {
		n = n2;
		for (int i = 0; i<n; i++)
			a[i] = 1;
		return 0;
	}
	int Zeros(int n2) {
		n = n2;
		for (int i = 0; i<n; i++)
			a[i] = 0;
		return 0;
	}

	Matrix find(int N) {
		Matrix r0n;
		r0n.Clear();
		r0n.n = 1;
		for (int i = 0; i<n; i++)
			if (a[i] == N) {
				r0n.a[r0n.n - 1] = i + 1;
				r0n.n++;
			}
		r0n.n--;
		return r0n;
	}

};

int R_make(Matrix *R, int *rshift) {
	Matrix vR, hR;
	vR.Clear();
	hR.Clear();
	vR.ones(N*(N + 1));
	hR.ones(N*(N + 1));
	for (int i = 1; i<N + 1; i++) {
		hR.a[(N + 1)*i - N - 1] = 0;
		hR.a[(N + 1)*i - 1] = 0;
	}
	R->n = 2 * N*(N + 1);
	for (int i = 0; i<R->n; i++)
		i<N*(N + 1) ? R->a[i] = vR.a[i] : R->a[i] = hR.a[i - N*N - N];
	*rshift = N*N + N;
	return 0;
}

int R_remove(Matrix *R) {
	Matrix r0n = R->find(1);
	int temp;
	//srand((int)time(0));
	temp = rand() % r0n.n;
	temp = r0n.a[temp];
	TempR = temp;
	printf("\n%3d This Volt_Matrix removes No.%d Resistor becomes to following Volt_Matrix:\n\n", temp,temp);
	R->a[temp - 1] = 0;
	return 0;
}

float LastV_find(Matrix_V *V, Matrix_V*V_Old, int R_Num, int *rshift) {
	float Res = 0;
	if (R_Num < *rshift)
		Res = V_Old->a[((R_Num - 1) / N)][((R_Num - 1) % N) + 1] - V_Old->a[((R_Num - 1) / N) + 1][((R_Num - 1) % N) + 1];
	else
		Res = V_Old->a[((R_Num - *rshift - 1) / (N + 1)) + 1][(R_Num - *rshift - 1) % (N + 1)] - V_Old->a[((R_Num - *rshift - 1) / (N + 1)) + 1][(R_Num - *rshift - 1) % (N + 1) + 1];
	*V_Old = *V;
	return Res;
}

Matrix_V Pop_find(Matrix *R, Matrix_V*V_Old, int x, int *rshift, int *EndF) {
	*EndF = 1;
	Matrix_V V;
	Matrix DeltaV;
	V.Clear();
	DeltaV.Clear();
	V.Zeros(N + 2, N + 2);
	for (int j = 0; j<N + 2; j++)
		V.a[0][j] = 1;
	DeltaV.Zeros(N*N);

	while (*EndF == 1) {
		float top;
		Matrix vMat, rMat, dotP;
		vMat.Clear();
		rMat.Clear();
		dotP.Clear();
		rMat.n = 4;
		vMat.n = 4;
		for (int i = 1; i<N + 1; i++)
			for (int j = 1; j<N + 1; j++) {

				rMat.a[0] = R->a[N*i - (N + 1) + j];
				rMat.a[1] = R->a[N*i - (N + 1) + j + N];
				rMat.a[2] = R->a[(N + 1)*(i - 1) + j - 1 + *rshift];
				rMat.a[3] = R->a[(N + 1)*(i - 1) + j + *rshift];

				float rPres = rMat.a[0] + rMat.a[1] + rMat.a[2] + rMat.a[3];
				if (rPres>0) {
					vMat.a[0] = V.a[i - 1][j];
					vMat.a[1] = V.a[i + 1][j];
					vMat.a[2] = V.a[i][j - 1];
					vMat.a[3] = V.a[i][j + 1];

					dotP.n = 4;
					for (int k = 0; k<dotP.n; k++)
						dotP.a[k] = rMat.a[k] * vMat.a[k];
					//
					top = dotP.a[0] + dotP.a[1] + dotP.a[2] + dotP.a[3];
					double Vbefore = V.a[i][j];
					V.a[i][j] = top / rPres;
					DeltaV.a[i - 1] = fabs(Vbefore - V.a[i][j]);
				}
				else V.a[i][j] = 0;
			}
		float Delta = 0;
		for (int i = 0; i<N*N; i++)
			Delta += DeltaV.a[i];
		if (Delta >0.0000005)
			*EndF = 1;
		else *EndF = 0;
	}
	TempV_Last = TempV_New;
	TempV_New = LastV_find(&V, V_Old, TempR, rshift);
	Matrix_V vFinal;
	vFinal.Clear();
	vFinal.n = V.n - 2;
	vFinal.m = V.m - 2;
	for (int i = 1; i < V.n - 1; i++)
		for (int j = 1; j < V.m - 1; j++)
			vFinal.a[i - 1][j - 1] = V.a[i][j];
	for (int i = 0; i < vFinal.n; i++) {
		for (int j = 0; j < vFinal.m; j++)
			printf("%.5f ",vFinal.a[i][j]);
		printf("\n");
	}
	
	return vFinal;
}

int Current_find(Matrix_V *V, Matrix *R, float *totalC, int *EndF) {
	float res = 1;
	*EndF = 0;
	*totalC = 0;

	Matrix_V Current, CPerRow, vUpper, vLower;
	Current.Clear();
	CPerRow.Clear();
	vUpper.Clear();
	vLower.Clear();
	Current.Zeros(N, 1);
	CPerRow.Zeros(N + 1, 1);
	vUpper.ones(1, N);
	vLower.Zeros(1, N);

	V->n += 2;
	for (int j = 0; j<N; j++)
		V->a[V->n - 1][j] = vLower.a[0][j];
	for (int i = V->n - 2; i>0; i--)
		for (int j = 0; j<N; j++)
			V->a[i][j] = V->a[i - 1][j];
	for (int j = 0; j<N; j++)
		V->a[0][j] = vUpper.a[0][j];

	for (int i = 0; i < N + 1; i++) {
		for (int j = 0; j < N; j++)
			Current.a[j][0] = ((V->a[i][j] - V->a[i + 1][j])*R->a[N*(i + 1) + j - N]) / res;
		Matrix_V temp;
		temp.Clear();
		temp = Current.sumcol();
		temp = temp.sumrow();
		CPerRow.a[i][0] = temp.a[0][0];
		if (CPerRow.a[i][0] < 0.0001) {
			*EndF = 1;
		}
		Current.Zeros(N, 1);
	}
	Matrix_V temp;
	temp.Clear();
	temp = CPerRow.sumcol();
	temp = temp.sumrow();
	*totalC = temp.a[0][0] / (N + 1);
	if (*EndF>0) *totalC = 0;
	return 0;
}

int main() {
	FILE*fp;
	int flag;
	do {
		TempR = 0;
		TempV_Last = 0;
		TempV_New = 0;
		Matrix R;
		Matrix Currentplt;
		R.Clear();
		Currentplt.Clear();
		int x = 1;
		int rshift = 0;
		int EndF = 0;
		R_make(&R, &rshift);
		printf("The Seqence of Moving Resistor :\n");
		while (EndF == 0) {
			Matrix_V V2, V_Old;
			V2.Clear();
			V2 = Pop_find(&R, &V_Old, N, &rshift, &EndF);
			float avgCpRow;
			Current_find(&V2, &R, &avgCpRow, &EndF);
			Currentplt.n = x;
			Currentplt.a[x - 1] = avgCpRow;
			x++;
			R_remove(&R);
		}
		Matrix_V V2, V_Old;
		V2.Clear(); V_Old.Clear();
		V2 = Pop_find(&R, &V_Old, N, &rshift, &EndF);
		Matrix ans;
		ans.Clear();
		ans = R.find(0);
		if ((fp = fopen("output.txt", "w")) == NULL) {
			printf("Error\n");
			return 0;
		}
		printf("\n");
		for (int i = 0; i<x - 1; i++) {
			fprintf(fp, "%.4f ", Currentplt.a[i]);
		//	printf("%d: %.4f\n",i+1,Currentplt.a[i]);

		}
		printf("The Volt on the Last Resistor is %.5f\n", TempV_Last);
		printf("Press Ctrl+Z to contiue the simulation\n");
		fclose(fp);
	} while (scanf("%d", &flag) == EOF);
	return 0;
}
