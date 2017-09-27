//#include "stm32f1xx.h"
#include <stm32f103xb.h>
#include <core_cm3.h>

//#include "system_stm32f1xx.h"
void InitSysTick (int period) {
    SysTick -> LOAD = period - 1;
    SysTick -> VAL  = period - 1;
    SysTick -> CTRL = SysTick_CTRL_ENABLE
                    | SysTick_CTRL_TICKINT
                    | SysTick_CTRL_CLKSOURCE;
}

void InitClock (void) {
    RCC -> CR |= RCC_CR_HSEON;
    while ((RCC -> CR & RCC_CR_HSERDY) == 0);

    RCC -> CFGR |= RCC_CFGR_PLLMULL6;
    RCC -> CFGR |= RCC_CFGR_PLLSRC;
    RCC -> CFGR |= RCC_CFGR_PLLXTPRE_HSE;
    RCC -> CFGR |= RCC_CFGR_PPRE2_DIV1;
    RCC -> CFGR |= RCC_CFGR_MCO_NOCLOCK;
    
    RCC -> CR |= RCC_CR_PLLON;
    while ( ( RCC -> CR & RCC_CR_PLLRDY) == 0);
    
    RCC -> CFGR |= RCC_CFGR_SW_PLL;
}

void InitLed (void) {
    RCC -> APB2ENR |= RCC_APB2ENR_IOPCEN;
    GPIOC -> CRH &= ~GPIO_CRH_CNF13;
    GPIOC -> CRH &= ~GPIO_CRH_MODE13;
    GPIOC -> CRH |=  GPIO_CRH_MODE13_0;
}


void touch13 (void) {
    GPIOC -> ODR ^= GPIO_ODR_ODR13;
}

void SystemInit (void) {
    InitClock();
    InitLed();
    InitSysTick (48e3);
}

void main (void) {
    while (1) {
        int x = 4e6;
        while (x--);
        //touch13();
    }
}

#define WAITER 5e2
int waiter=WAITER;
void SysTick_Handler (void) {
    if (waiter--)
        ;
    else {
        waiter=WAITER;
        touch13();
    }
}
