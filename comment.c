#include <inttypes.h>
#include "types.h"
#include "values.h"
#include "runtime.h"
#include "print.h"

extern val_t* to;
extern val_t* from;

val_t* collect_garbage(val_t* rsp, val_t *rbp, val_t* rbx) {

    printf("rsp: %p\n", rsp);
    printf("rbp: %p\n", rbp);
    printf("rbx: %p\n", rbx);
    printf("to space: %p %ld\n", to, to);
    printf("from space: %p %ld\n\n", from, from);
    
    // for (int i = 0; i < 4; ++i) {
    //     val_t val = *rsp;
    //     type_t t = val_typeof(val);
    //     printf("Addr: %p\tVal: %lx\tType: %d\tInterior: ", rsp, val, t);
    //     print_result(val);
    //     printf("\n");
    //     fflush(stdout);
    //     rsp++;
    // }

    // for (int i = 0; i < 4; ++i) {
    //     val_t val = *rbp;
    //     type_t t = val_typeof(val);
    //     printf("Addr: %p\tVal: %lx\tType: %d\tInterior: ", rbp, val, t);
    //     print_result(val);
    //     printf("\n");
    //     rbp--;
    // }

    while (rsp < rbp) {
        val_t val = *rsp;
        type_t type = val_typeof(val);
        val_box_t* box_pointer;
        val_cons_t* cons_pointer;

        printf("rsp: %p\tVal: %lx\tType: %d", rsp, val, type);

        if (type == T_BOX) {
            box_pointer = val_unwrap_box(val);
            printf("\tPointer: %p", box_pointer);
        }
        else if (type == T_CONS) {
            cons_pointer = val_unwrap_cons(val);
            printf("\tPointer: %p", cons_pointer);
        }

        printf("\tInterior: ");
        print_result(val);
        printf("\n");
        ++rsp;
    }

    return rbx;
}


###########################################################################################


#include <inttypes.h>
#include "types.h"
#include "values.h"
#include "runtime.h"
#include "print.h"
#include <assert.h>

extern val_t* to;
extern val_t* from;
extern type_t* types;

int front_queue = 0, end_queue = 0;

void print_memory(val_t* base, val_t* top, char* mem);

void process_element(val_t interior, type_t interior_type, val_t* curr, val_t* rbx, val_t** rbx_double);

val_t* collect_garbage(val_t* rsp, val_t *rbp, val_t* rbx) {

    printf("rsp: %p\n", rsp);
    printf("rbp: %p\n", rbp);
    printf("rbx: %p\n", rbx);
    printf("to space: %p %ld\n", to, to);
    printf("from space: %p %ld\n\n", from, from);

    printf("Stack:\n");
    print_memory(rsp, rbp, "rsp");
    printf("From Space:\n");
    print_memory(from, rbx, "heap");
    printf("\n");
    

    assert( rbx-from >= 0 && rbx-from < heap_size );
    val_t* old_rbx = rbx;
    val_t* old_rsp = rsp;
    rbx = to;


    // first phase through stack
    while (rsp < rbp) {
        val_t val = *rsp;
        type_t type = val_typeof(val);

        val_box_t* box_pointer;
        val_cons_t* cons_pointer;
        val_vect_t* vect_pointer;
        val_str_t* str_pointer;

        if (type == T_BOX) {
            box_pointer = val_unwrap_box(val);

            val_t forwarding_val = *((val_t*) box_pointer);
            val_t* forwarding_pointer = (val_t*) val_unwrap_box(forwarding_val);

            if (val_typeof(forwarding_val) == T_BOX && forwarding_pointer - to >= 0 && forwarding_pointer - to < heap_size) {                printf("\nSet To Forwarding Pointer\n");
                *rsp = forwarding_val;
            }
            else {
                // copy the box from from_space to to_space
                val_box_t* box_heap = (val_box_t*) rbx;
                *box_heap = *box_pointer;

                // set rsp to box in to_space
                *rsp = val_wrap_box(box_heap);

                // leave a forwarding pointer
                val_t* forwarding_ptr = (val_t*) box_pointer;
                *forwarding_ptr = val_wrap_box(box_heap);

                // increment the rbx pointer
                box_heap++;
                rbx = (val_t*) box_heap;

                // enqueue the type
                types[end_queue] = T_BOX;
                end_queue++;
            }
        }
        else if (type == T_CONS) {
            cons_pointer = val_unwrap_cons(val);

            val_t forwarding_val = *((val_t*) cons_pointer);
            val_t* forwarding_pointer = (val_t*) val_unwrap_cons(forwarding_val);

            if (val_typeof(forwarding_val) == T_CONS && forwarding_pointer - to >= 0 && forwarding_pointer - to < heap_size) {
                *rsp = forwarding_val;
            }
            else {
                // copy the cons from from_space to to_space
                val_cons_t* cons_heap = (val_cons_t*) rbx;
                *cons_heap = *cons_pointer;

                // set rsp to cons in to_space
                *rsp = val_wrap_cons(cons_heap);

                // leave a forwarding pointer
                val_t* forwarding_ptr = (val_t*) cons_pointer;
                *forwarding_ptr = val_wrap_cons(cons_heap);

                // increment the rbx pointer
                cons_heap++;
                rbx = (val_t*) cons_heap;

                // enqueue the type
                types[end_queue] = T_CONS;
                end_queue++;
            }

        }
        else if (type >= T_VOID && type <= T_EMPTY) {
            ;
        }

        ++rsp;
    }

    // second phase through heap
    // while (to < rbx) {
    //     type_t type = types[front_queue];
    //     front_queue++;

    //     val_box_t* box_pointer;
    //     val_cons_t* cons_pointer;
    //     val_vect_t* vect_pointer;
    //     val_str_t* str_pointer;
        
    //     if (type == T_BOX) {
    //         box_pointer = (val_box_t*) to;
    //         val_t val = box_pointer->val;
    //         type_t type = val_typeof(val);
            
    //         if (type == T_BOX) {
                
    //         }
    //     }

    //     ++to;
    // }

    printf("\nAfter First Phase\nQueue: ");
    for (int i = front_queue; i < end_queue; ++i) {
        printf("%d ", types[i]);
    }
    printf("\nStack:\n");
    print_memory(old_rsp, rbp, "stack");
    printf("From Space:\n");
    print_memory(from, old_rbx, "heap");
    printf("To Space:\n");
    print_memory(to, rbx, "heap");

    val_t* curr = to;

    while (curr < rbx) {
        type_t type = types[front_queue];
        front_queue++;

        val_box_t* box_pointer;
        val_cons_t* cons_pointer;
        val_vect_t* vect_pointer;
        val_str_t* str_pointer;

        if (type == T_BOX) {
            val_t interior = ((val_box_t*) curr)->val;
            type_t interior_type = val_typeof(interior);

            process_element(interior, interior_type, curr, rbx, &rbx);

            // if (interior_type == T_BOX) {
            //     box_pointer = val_unwrap_box(interior);

            //     val_t forwarding_val = *((val_t*) box_pointer);
            //     val_t* forwarding_pointer = (val_t*) val_unwrap_box(forwarding_val);

            //     if (val_typeof(forwarding_val) == T_BOX && forwarding_pointer-to >= 0 && forwarding_pointer-to < heap_size) {
            //         *curr = forwarding_val;
            //     }
            //     else {
            //         // copy the box from from_space to to_space
            //         val_box_t* box_heap = (val_box_t*) rbx;
            //         *box_heap = *box_pointer;
                    
            //         // set curr to box in to_space
            //         *curr = val_wrap_box(box_heap);

            //         // leave a forwarding pointer
            //         val_t* forwarding_ptr = (val_t*) box_pointer;
            //         *forwarding_ptr = val_wrap_box(box_heap);

            //         // increment the rbx pointer
            //         box_heap++;
            //         rbx = (val_t*) box_heap;

            //         // enqueue the type
            //         types[end_queue] = T_BOX;
            //         end_queue++;
            //     }
            // }
            // else if (interior_type == T_CONS) {
            //     cons_pointer = val_unwrap_cons(interior);

            //     val_t forwarding_val = *((val_t*) cons_pointer);
            //     val_t* forwarding_pointer = (val_t*) val_unwrap_cons(forwarding_val);

            //     if (val_typeof(forwarding_val) == T_CONS && forwarding_pointer-to >= 0 && forwarding_pointer-to < heap_size) {
            //         *curr = forwarding_val;
            //     }
            //     else {
            //         // copy the cons from from_space to to_space
            //         val_cons_t* cons_heap = (val_cons_t*) rbx;
            //         *cons_heap = *cons_pointer;
                    
            //         // set curr to cons in to_space
            //         *curr = val_wrap_cons(cons_heap);

            //         // leave a forwarding pointer
            //         val_t* forwarding_ptr = (val_t*) cons_pointer;
            //         *forwarding_ptr = val_wrap_cons(cons_heap);

            //         // increment the rbx pointer
            //         cons_heap++;
            //         rbx = (val_t*) cons_heap;

            //         // enqueue the type
            //         types[end_queue] = T_CONS;
            //         end_queue++;
            //     }
            // }
            // else {
            //     ;
            // }

            val_box_t* curr_box = (val_box_t*) curr;
            curr_box++;
            curr = (val_t*) curr_box;
        }
        else if (type == T_CONS) {
            val_t first = ((val_cons_t*) curr)->fst;
            val_t second = ((val_cons_t*) curr)->snd;
            type_t first_type = val_typeof(first);
            type_t second_type = val_typeof(second);

            process_element(first, first_type, &(((val_cons_t*) curr)->fst), rbx, &rbx);
            process_element(second, second_type, &(((val_cons_t*) curr)->snd), rbx, &rbx);

            val_cons_t* curr_cons = (val_cons_t*) curr;
            curr_cons++;
            curr = (val_t*) curr_cons;
        }
    }

    printf("\nAfter Garbage Collection\nStack:\n");
    print_memory(old_rsp, rbp, "stack");
    printf("From Space:\n");
    print_memory(from, old_rbx, "heap");
    printf("To Space:\n");
    print_memory(to, rbx, "heap");

    return rbx;
}


void process_element(val_t interior, type_t interior_type, val_t* curr, val_t* rbx, val_t** rbx_double) {

    val_box_t* box_pointer;
    val_cons_t* cons_pointer;
    val_vect_t* vect_pointer;
    val_str_t* str_pointer;

    if (interior_type == T_BOX) {
        box_pointer = val_unwrap_box(interior);

        val_t forwarding_val = *((val_t*) box_pointer);
        val_t* forwarding_pointer = (val_t*) val_unwrap_box(forwarding_val);

        if (val_typeof(forwarding_val) == T_BOX && forwarding_pointer-to >= 0 && forwarding_pointer-to < heap_size) {
            *curr = forwarding_val;
        }
        else {
            // copy the box from from_space to to_space
            val_box_t* box_heap = (val_box_t*) rbx;
            *box_heap = *box_pointer;
            
            // set curr to box in to_space
            *curr = val_wrap_box(box_heap);

            // leave a forwarding pointer
            val_t* forwarding_ptr = (val_t*) box_pointer;
            *forwarding_ptr = val_wrap_box(box_heap);

            // increment the rbx pointer
            box_heap++;
            *rbx_double = (val_t*) box_heap;

            // enqueue the type
            types[end_queue] = T_BOX;
            end_queue++;
        }
    }
    else if (interior_type == T_CONS) {
        cons_pointer = val_unwrap_cons(interior);

        val_t forwarding_val = *((val_t*) cons_pointer);
        val_t* forwarding_pointer = (val_t*) val_unwrap_cons(forwarding_val);

        if (val_typeof(forwarding_val) == T_CONS && forwarding_pointer-to >= 0 && forwarding_pointer-to < heap_size) {
            *curr = forwarding_val;
        }
        else {
            // copy the cons from from_space to to_space
            val_cons_t* cons_heap = (val_cons_t*) rbx;
            *cons_heap = *cons_pointer;
            
            // set curr to cons in to_space
            *curr = val_wrap_cons(cons_heap);

            // leave a forwarding pointer
            val_t* forwarding_ptr = (val_t*) cons_pointer;
            *forwarding_ptr = val_wrap_cons(cons_heap);

            // increment the rbx pointer
            cons_heap++;
            *rbx_double = (val_t*) cons_heap;

            // enqueue the type
            types[end_queue] = T_CONS;
            end_queue++;
        }
    }
    else {
        ;
    }
}





void print_memory(val_t* base, val_t* top, char* mem) {
    // mem = "rsp" or "heap"

    while (base < top) {
        val_t val = *base;
        type_t type = val_typeof(val);

        val_box_t* box_pointer;
        val_cons_t* cons_pointer;
        val_vect_t* vect_pointer;
        val_str_t* str_pointer;

        printf("%s: %p\tVal: %lx\tType: %d", mem, base, val, type);

        if (type == T_BOX) {
            box_pointer = val_unwrap_box(val);
            printf("\tPointer: %p", box_pointer);
        }
        else if (type == T_CONS) {
            cons_pointer = val_unwrap_cons(val);
            printf("\tPointer: %p", cons_pointer);
        }
        else if (type == T_VECT) {
            vect_pointer = val_unwrap_vect(val);
            printf("\tPointer: %p", vect_pointer);
        }
        else if (type == T_STR) {
            str_pointer = val_unwrap_str(val);
            printf("\tPointer: %p", str_pointer);
        }

        printf("\tInterior: ");
        print_result(val);
        printf("\n");
        ++base;
    }
}