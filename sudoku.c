#include <stdio.h>
#include <signal.h>
#include <stdlib.h>

int static_init[10][10] = {
    { 0,         0, 0, 0,  0, 0, 0,  0, 0, 0 },
    /* fill the below matrix with the solution space */
              /* 1  2  3   4  5  6   7  8  9*/
    { 0, /* 1 */ 7, 0, 0,  0, 9, 0,  0, 0, 3 },
    { 0, /* 2 */ 0, 0, 5,  8, 0, 2,  6, 0, 0 },
    { 0, /* 3 */ 0, 8, 0,  3, 0, 1,  0, 9, 0 },
    
    { 0, /* 4 */ 0, 5, 0,  7, 0, 4,  0, 1, 0 },
    { 0, /* 5 */ 3, 0, 0,  0, 0, 0,  0, 0, 4 },
    { 0, /* 6 */ 0, 4, 0,  5, 0, 9,  0, 8, 0 },

    { 0, /* 7 */ 0, 2, 0,  9, 0, 8,  0, 5, 0 },
    { 0, /* 8 */ 0, 0, 9,  6, 0, 7,  4, 0, 0 },
    { 0, /* 9 */ 5, 0, 0,  0, 2, 0,  0, 0, 8 } 
};


struct cell_state {
        char valid[10];
        char fixed;
        char no_valids;
};
struct egg {
    struct cell_state ko[10][10];
} ;
int need_disp = 0;
int rand_num;

void sig(int x)
{
    need_disp = 1;
}
void crack_ko(struct egg egg,int x,int y);

void disp_ko(struct egg egg)
{
    int x,y;
    static int times ;

    for(y=1;y<=9;y++) {
        for(x=1;x<=9;x++) {
            printf("%d ",egg.ko[x][y].fixed);
        }
        printf("\n");
    }

    printf("\n");
    need_disp = 0;
}

void fix_and_effects(struct egg *egg,int x,int y,int val)
{
    int x_base,y_base;
    int i,j,k;

    if(! egg->ko[x][y].fixed) {
        egg->ko[x][y].fixed = val;
        egg->ko[x][y].no_valids = 1;
        for(k=1;k<=9;k++)
            if(k != val)
                egg->ko[x][y].valid[k] = 0;
            else
                egg->ko[x][y].valid[k] = 1;
    
        /* row */
        for(i=1;i<=9;i++) {
            if(!egg->ko[i][y].fixed && egg->ko[i][y].valid[val]) {
                egg->ko[i][y].valid[val] = 0;
                egg->ko[i][y].no_valids--;
            }
        }
        /* column */
        for(j=1;j<=9;j++) {
            if(!egg->ko[x][j].fixed && egg->ko[x][j].valid[val]) {
                egg->ko[x][j].valid[val] = 0;
                egg->ko[x][j].no_valids--;
            }
        }
        /* block */
        x_base = ((x - 1)/3)*3 + 1;
        y_base = ((y - 1)/3)*3 + 1;
        for(i=x_base;i<x_base+3;i++) {
            for(j=y_base;j<y_base+3;j++) {
                if(!egg->ko[i][j].fixed && egg->ko[i][j].valid[val]) {
                    egg->ko[i][j].valid[val] = 0;
                    egg->ko[i][j].no_valids--;
                }
            }
        }
    }

    if(need_disp)
        disp_ko(*egg);
}

void _crack_ko(struct egg egg,int x,int y,int val)
{
    fix_and_effects(&egg,x,y,val);
    x++;
    if(x == 10) {
        x = 1;
        y++;
        if(y == 10) {
            disp_ko(egg);
            return ;
        }
    }
    crack_ko(egg,x,y);
}

void crack_ko(struct egg egg,int x,int y)
{
    int val;

    for(val = 1; val <= 9; val++)
        if(egg.ko[x][y].valid[val])
            _crack_ko(egg,x,y,val);
}

int main()
{
    struct egg egg;
    int i,j,k;

    memset(&egg,0,sizeof(egg));
    for(i=1;i<=9;i++) {
        for(j=1;j<=9;j++) {
            egg.ko[i][j].fixed = 0;
            for(k=1;k<=9;k++) {
                egg.ko[i][j].valid[k] = 1;
            }
            egg.ko[i][j].no_valids = 9;
        }
    }

    for(i=1;i<=9;i++) {
        for(j=1;j<=9;j++) {
            if(static_init[j][i]) {
                fix_and_effects(&egg,i,j,static_init[j][i]);
            }
        }
    }

//    srand(time(NULL));
//    rand_num = rand() % 1000000;
//    printf("Rand: %d\n",rand_num);
    signal(SIGINT,sig);
    need_disp = 0;
    crack_ko(egg,1,1);

    return 0;
}
