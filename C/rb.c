#include <stdio.h>
#include <stdlib.h>

/*  
 *  Author      : Collen Roller
 *  Date        : January 29th, 2015 
 */

/*
 *  The purpose of this program is to input three arguments
 *  Hex value, position to replace, replacement value
 *  Once the arguments are inputed into the program, the replace_byte
 *  method will replace the bytes of x and return the next unsigned value
 *  
 *  To Compile and create object file : gcc -o rb rb.c
 *  To Run : ./rb <value> <ith position> <replacement>
 *  EX) : ./rb 0x12345678 2 0xAB
 *
 *  I've set the program up to print the original as well as the result value
 * 
 */

/*
 *  Purpose     :   replace the bytes in x at the ith position with b
 * 
 *  Parameters  :   unsigned x         :   value to be modified
 *                  int i              :   position to change 
 *                  unsigned char b    :   replacing value  
 *  
 *  Returns     :   An unsigned value representing x, with the bytes at i
 *                  replaced with b
 *
 */
unsigned replace_byte (unsigned x, int i, unsigned char b){

    int mask = 0xff << (i << 3);
    int shift = b << (i << 3);
    unsigned result = (~mask & x) | shift;
    return result;

}

/*
 *  Purpose     :   Main method that drives this program
 *                  3 arguments must be inputed into the program
 *                  Value - value to be modified (argv[1])
 *                  Position - positon to change in value (argv[2])
 *                  Replacing value - replacing value 
 */
int main(int argc, char * argv[]){
        
    if(argc != 4){
        printf("%s\n", "Incorrect number of arguments");
        printf("%s\n", "value, which byte to replace, the replacement");
        return 0;
    } 

    unsigned x = (unsigned)strtol(argv[1],NULL,16);
    int i = atoi(argv[2]);
    unsigned char b = (unsigned char)strtol(argv[3],NULL,16);

    unsigned result = replace_byte(x,i,b);

    printf("%s %x\n", "Original value :", x);
    printf("%s %x\n", "New Value :", result);

    return 0;

}
