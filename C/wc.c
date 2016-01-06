#include <stdio.h>
#define WORDMAX 50

/*  
 *  Author      : Collen Roller
 *  Date        : January 26th, 2015 
 */

/*
 * The purpose of this program is to take an arbitrary amount of  
 * command line strings, and parse them by (' ') || ('\n') || ('\t')
 * Each string will be printed to output along with a integer, which
 * conveys the length of each string. For this assignment, I am using
 * a wordBuf that has a defined size of 30 characters. I assume that
 * words given to this program won't be over the length of 50 characters
 *
 * To Compiler and create object file : gcc -o wc wc.c
 * To Run : ./wc
 * User then can input an arbitrary amount of words, which will be 
 * printed along with a word length directly following the word
 *
 * Text files can be redirected into the program after compiled
 * ie) ./wc < test.txt
 *
 * Output can also be redirected using > operator
 * ie) ./wc > output.txt
 *
 * These functions also work with both!
 * ie) ./wc < text.txt > output.txt
 * 
 */


/*
 * Purpose : Output characters in wordBuf until a \0 character is hit
 *           Then the length of the string is printed to output
 * Parameters : char * wordBuf : array of characters holding a word 
 *            : int count : the number of characters in wordBuf  
 */
void print_word(char * wordBuf, int count){

    int i = 0;
    char cur = wordBuf[i];
    while(cur != '\0'){
        printf("%c", wordBuf[i]);
        i++;
        cur = wordBuf[i]; 
    }
    printf(" %d\n", count);
}

/*
 * Purpose : Main method that drives this program, which seperates words
 *           by ' ' , '\n' and '\t' and sends them to the print_word function
 *           to be sent to output
 */
int main(){

    char wordBuf[WORDMAX];
    int count = 0;
    int cur;
    while((cur = getchar()) != EOF) {
        
        if(cur == '\t' || cur == ' ' || cur == '\n'){
            if(count > 0){
                wordBuf[count] = '\0'; /*add null character*/
                print_word(wordBuf, count);
                count = 0;
            }
        }else{
            wordBuf[count] = (char) cur;
            count++;
        }
    }
    return 0;
}
