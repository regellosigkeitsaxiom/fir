#include <stm32l432xx.h>
#include <core_cm4.h>
#include "system.c"

void __attribute__((optimize("O0"))) badDelay ( uint32_t x ) {
  while (x--);
}

void initLeds(void) {
  RCC -> AHB2ENR |= RCC_AHB2ENR_GPIOBEN; //Enable GPIO port B

  GPIOB -> MODER &= ~GPIO_MODER_MODE3_Msk; //Clear MODE bits for pin 3
  GPIOB -> MODER |= ( 0b01 << GPIO_MODER_MODE3_Pos); //Mode to general-purpose IO
  GPIOB -> OTYPER &= ~GPIO_OTYPER_OT_3; //Push-pull (1 is open-drain)
  GPIOB -> PUPDR &= ~GPIO_PUPDR_PUPD3_Msk; //No pull-ups (01 is UP, 10 is DOWN, 11 is both)
}

int main(void)
{
  initLeds();
  while (1)
  {
    badDelay(1e6);
    GPIOB -> ODR ^= GPIO_ODR_OD3; //Toggle pin 3
  }
}
