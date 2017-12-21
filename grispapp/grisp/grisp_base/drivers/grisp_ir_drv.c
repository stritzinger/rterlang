/* grisp_ir_drv.c */

#include <assert.h>
#include <fcntl.h>
#include <rtems.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/ioctl.h>

#include <bsp.h>
#include <bsp/pin-config.h>

#include "erl_driver.h"
#include "sys.h"

static Pin pins[] =
  {
    {PIO_PC12, PIOC, ID_PIOC, PIO_INPUT, PIO_IT_FALL_EDGE},    /* 0: gpio1 pin1 */
    {PIO_PC13, PIOC, ID_PIOC, PIO_INPUT, PIO_DEFAULT},    /* 1: gpio1 pin2 */
    {PIO_PA21, PIOA, ID_PIOA, PIO_INPUT, PIO_DEFAULT},    /* 2: gpio1 pin3 */
    {PIO_PD30, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},    /* 3: gpio1 pin4 */
    {PIO_PD0, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},     /* 4: gpio2 pin1 */
    {PIO_PD1, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},     /* 5: gpio2 pin2 */
    {PIO_PD2, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},     /* 6: gpio2 pin3 */
    {PIO_PD3, PIOD, ID_PIOD, PIO_INPUT, PIO_DEFAULT},     /* 7: gpio2 pin4 */
    {PIO_PA24, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 8:  led1 r */
    {PIO_PA17, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 9:  led1 g */
    {PIO_PA23, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 10: led1 b */
    {PIO_PA13, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 11: led2 r */
    {PIO_PA5 , PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 12  led2 g */
    {PIO_PA11, PIOA, ID_PIOA, PIO_OUTPUT_0, PIO_DEFAULT}, /* 13: led2 b */
    {PIO_PA6 , PIOA, ID_PIOA, PIO_INPUT, PIO_PULLUP},     /* 14: jumper 1 */
    {PIO_PD9 , PIOD, ID_PIOD, PIO_INPUT, PIO_PULLUP},     /* 15: jumper 2 */
    {PIO_PC17, PIOC, ID_PIOC, PIO_INPUT, PIO_PULLUP},     /* 16: jumper 3 */
    {PIO_PD11, PIOD, ID_PIOD, PIO_INPUT, PIO_PULLUP},     /* 17: jumper 4 */
    {PIO_PC10, PIOC, ID_PIOC, PIO_INPUT, PIO_PULLUP},     /* 18: jumper 5 */
    {PIO_PA9, PIOA, ID_PIOA, PIO_INPUT, PIO_DEFAULT},     /* 19: spi1 pin 9 */
    {PIO_PA10, PIOA, ID_PIOA, PIO_INPUT, PIO_DEFAULT},    /* 20: spi1 pin10 */
    {PIO_PD12C_SPI0_NPCS2, PIOD, ID_PIOD,
     PIO_PERIPH_C, PIO_DEFAULT},                          /* 21: SS1 */
    {PIO_PD27B_SPI0_NPCS3, PIOD, ID_PIOD,
     PIO_PERIPH_B, PIO_DEFAULT}                           /* 22: SS2 */

  };

struct grisp_ir_data {
    ErlDrvPort port;
    int fd[2];
    rtems_id tid;
};

ErlDrvData grisp_ir_start(ErlDrvPort port, char* command);
void grisp_ir_stop(ErlDrvData data);
void grisp_ir_ready_input(ErlDrvData data, ErlDrvEvent event);
void grisp_ir_stop_select(ErlDrvEvent event, void* reserved);
rtems_task pin_trigger(rtems_task_argument arg);
void grisp_ir_handle(const Pin *pPIn, void *arg);

ErlDrvEntry grisp_ir_driver_entry = {
    NULL,
    grisp_ir_start,
    grisp_ir_stop,
    NULL, // output
    grisp_ir_ready_input, // 
    NULL,
    "grisp_ir_drv",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    grisp_ir_stop_select
};

const Pin *triggerpin = pins;

ErlDrvData grisp_ir_start(ErlDrvPort port, char *command)
{
    struct grisp_ir_data *data = (struct grisp_ir_data *)driver_alloc(sizeof(struct grisp_ir_data));
    rtems_name taskname;
    
    if(pipe(data->fd) != 0)
        return ERL_DRV_ERROR_GENERAL;
    
    data->port = port;
    
    /* Setup Pins for Interrupt */
    //PIO_SysInitializeInterrupts(); //priority 10?
    PIO_ConfigureIt(triggerpin,
    	&grisp_ir_handle, data); // *arg is unused
    PIO_EnableIt(triggerpin);
    
    
    /* Start task to receive interrupts */
    taskname = rtems_build_name('G','R','I','R');
    if (rtems_task_create(taskname, 8, 1024*8, RTEMS_INTERRUPT_LEVEL(0), RTEMS_GLOBAL, &(data->tid)) != RTEMS_SUCCESSFUL)
        return ERL_DRV_ERROR_GENERAL;
    if (rtems_task_start(data->tid, &pin_trigger, (rtems_task_argument) data)  != RTEMS_SUCCESSFUL)
        return ERL_DRV_ERROR_GENERAL;
    
    driver_select(data->port, (ErlDrvEvent)data->fd[0], ERL_DRV_READ, 1);
    return (ErlDrvData) data;
}

rtems_task pin_trigger(rtems_task_argument arg){
    struct grisp_ir_data *data = (struct grisp_ir_data *) arg;
    rtems_event_set eset;
    char payload = 1;
    while(rtems_event_receive(RTEMS_EVENT_5, RTEMS_WAIT, 0, &eset)  != RTEMS_SUCCESSFUL){
        write(data->fd[1], &payload, sizeof(char));
    }
}

/* ISR */
void grisp_ir_handle(const Pin *pPIn, void *arg){
    struct grisp_ir_data *data = (struct grisp_ir_data *) arg;
    rtems_event_send(data->tid, RTEMS_EVENT_5);
}

/* Erlang select callback */
void grisp_ir_ready_input(ErlDrvData arg, ErlDrvEvent event){
    struct grisp_ir_data *data = (struct grisp_ir_data *) arg;
    char res;
    while(read(data->fd[0], &res, sizeof(char)) > 0){
        driver_output(data->port, &res, 1);
    }
}

void grisp_ir_stop(ErlDrvData arg){
    struct grisp_ir_data *data = (struct grisp_ir_data *) arg;
    PIO_DisableIt(triggerpin);
    driver_select(data->port, (ErlDrvEvent)data->fd[0], ERL_DRV_USE, 0);
    close(data->fd[0]);
    close(data->fd[1]);
    free(data);
}

void grisp_ir_stop_select(ErlDrvEvent event, void* reserved){
    close((int)event);
}
