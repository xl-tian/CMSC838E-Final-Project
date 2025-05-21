#include <inttypes.h>
#include "types.h"
#include "values.h"
#include "runtime.h"
#include "print.h"
#include <assert.h>
#include <stdlib.h>


extern val_t* to;
extern val_t* from;
extern type_t* types;

val_t** stk;

int top_stk = 0;

int front_queue = 0, end_queue = 0;

void print_memory(val_t* base, val_t* top, char* mem);

void process_element(val_t interior, type_t interior_type, val_t* curr, val_t* rbx, val_t** rbx_double);

void mark_hot(val_t* addr) {
    printf("Addr: %p\n", addr);

    return;
}

val_t* collect_garbage(val_t* rsp, val_t *rbp, val_t* rbx) {

    stk = malloc(sizeof(val_t*) * heap_size);

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

    front_queue = 0;
    end_queue = 0;


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

            if (val_typeof(forwarding_val) == T_BOX && forwarding_pointer - to >= 0 && forwarding_pointer - to < heap_size) {
                // printf("\nSet To Forwarding Pointer\n");
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

                // push ptr to box into stk
                stk[top_stk] = rbx;
                top_stk++;

                // increment the rbx pointer
                box_heap++;
                rbx = (val_t*) box_heap;

                // enqueue the type
                // types[end_queue] = T_BOX;
                // end_queue++;
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

                // push ptr to fst and snd into stk
                stk[top_stk] = &(cons_heap->fst);
                top_stk++;
                stk[top_stk] = &(cons_heap->snd);
                top_stk++;

                // increment the rbx pointer
                cons_heap++;
                rbx = (val_t*) cons_heap;

                // // enqueue the type
                // types[end_queue] = T_CONS;
                // end_queue++;
            }
        }
        else if (type == T_VECT) {
            vect_pointer = val_unwrap_vect(val);

            val_t forwarding_val = *((val_t*) vect_pointer);
            val_t* forwarding_pointer = (val_t*) val_unwrap_vect(forwarding_val);
            if (val_typeof(forwarding_val) == T_VECT && forwarding_pointer - to >= 0 && forwarding_pointer - to < heap_size) {
                *rsp = forwarding_val;
            }
            else {
                // copy the vect from from_space to to_space
                val_vect_t* vect_heap = (val_vect_t*) rbx;
                vect_heap->len = vect_pointer->len;
                uint64_t len = vect_pointer->len;
                for (int i = 0; i < len; ++i) {
                    vect_heap->elems[i] = vect_pointer->elems[i];
                }

                // set rsp to vect in to_space
                *rsp = val_wrap_vect(vect_heap);

                // leave a forwarding pointer
                val_t* forwarding_ptr = (val_t*) vect_pointer;
                *forwarding_ptr = val_wrap_vect(vect_heap);

                // push ptr to each elem into stk
                for (int i = 0; i < len; ++i) {
                    stk[top_stk] = &(vect_heap->elems[i]);
                    top_stk++;
                }

                // increment the rbx pointer
                rbx += (1+len);

                // enqueue the type
                // types[end_queue] = T_VECT;
                // end_queue++;
            }
        }
        else if (type == T_STR) {
            str_pointer = val_unwrap_str(val);

            val_t forwarding_val = *((val_t*) str_pointer);
            val_t* forwarding_pointer = (val_t*) val_unwrap_str(forwarding_val);
            if (val_typeof(forwarding_val) == T_STR && forwarding_pointer - to >= 0 && forwarding_pointer - to < heap_size) {
                *rsp = forwarding_val;
            }
            else {
                // copy the str from from_space to to_space
                val_str_t* str_heap = (val_str_t*) rbx;
                str_heap->len = str_pointer->len;
                uint64_t len = str_pointer->len;
                len += (len % 2);
                for (int i = 0; i < len; ++i) {
                    str_heap->codepoints[i] = str_pointer->codepoints[i];
                }

                // set rsp to str in to_space
                *rsp = val_wrap_str(str_heap);

                // leave a forwarding pointer
                val_t* forwarding_ptr = (val_t*) str_pointer;
                *forwarding_ptr = val_wrap_str(str_heap);

                // increment the rbx pointer
                rbx += (1+len/2);

                // enqueue the type
                types[end_queue] = T_STR;
                end_queue++;
            }
        }
        else if (type == T_PROC) {
            val_t* proc_pointer = (val_t *)(val ^ proc_type_tag);

            val_t forwarding_val = *proc_pointer;
            val_t* forwarding_pointer = (val_t *)(forwarding_val ^ proc_type_tag);
            if (val_typeof(forwarding_val) == T_PROC && forwarding_pointer - to >= 0 && forwarding_pointer - to < heap_size) {
                *rsp = forwarding_val;
            }
            else {
                // get # of arguments
                val_t* proc_loc = (val_t*) (*proc_pointer);
                uint64_t len = (uint64_t) *(proc_loc-1);

                // copy proc location to to_space
                *rbx = *proc_pointer;

                // set rsp to proc in to_space
                *rsp = ((val_t) rbx) | proc_type_tag;

                // leave a forwarding pointer
                *proc_pointer = ((val_t) rbx) | proc_type_tag;

                // copy the arguments
                rbx++;
                proc_pointer++;
                for (int i = 0; i < len; ++i) {
                    *rbx = *proc_pointer;

                    stk[top_stk] = rbx;
                    top_stk++;

                    ++rbx;
                    ++proc_pointer;
                }
                
                // enqueue the type
                types[end_queue] = T_PROC;
                end_queue++;  
            }
        }
        else if (type >= T_VOID && type <= T_EMPTY) {
            ;
        }

        while (top_stk != 0) {
            // printf("\nIn While Loop\n");
            val_t* curr = stk[top_stk-1];
            val_t interior = *curr;
            type_t interior_type = val_typeof(interior);
            top_stk--;

            process_element(interior, interior_type, curr, rbx, &rbx);
        }

        ++rsp;
    }

    // second phase through heap

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



    // val_t* curr = to;

    // while (curr < rbx) {
    //     type_t type = types[front_queue];
    //     front_queue++;

    //     val_box_t* box_pointer;
    //     val_cons_t* cons_pointer;
    //     val_vect_t* vect_pointer;
    //     val_str_t* str_pointer;

    //     if (type == T_BOX) {
    //         val_t interior = ((val_box_t*) curr)->val;
    //         type_t interior_type = val_typeof(interior);

    //         process_element(interior, interior_type, curr, rbx, &rbx);

    //         // if (interior_type == T_BOX) {
    //         //     box_pointer = val_unwrap_box(interior);

    //         //     val_t forwarding_val = *((val_t*) box_pointer);
    //         //     val_t* forwarding_pointer = (val_t*) val_unwrap_box(forwarding_val);

    //         //     if (val_typeof(forwarding_val) == T_BOX && forwarding_pointer-to >= 0 && forwarding_pointer-to < heap_size) {
    //         //         *curr = forwarding_val;
    //         //     }
    //         //     else {
    //         //         // copy the box from from_space to to_space
    //         //         val_box_t* box_heap = (val_box_t*) rbx;
    //         //         *box_heap = *box_pointer;
                    
    //         //         // set curr to box in to_space
    //         //         *curr = val_wrap_box(box_heap);

    //         //         // leave a forwarding pointer
    //         //         val_t* forwarding_ptr = (val_t*) box_pointer;
    //         //         *forwarding_ptr = val_wrap_box(box_heap);

    //         //         // increment the rbx pointer
    //         //         box_heap++;
    //         //         rbx = (val_t*) box_heap;

    //         //         // enqueue the type
    //         //         types[end_queue] = T_BOX;
    //         //         end_queue++;
    //         //     }
    //         // }
    //         // else if (interior_type == T_CONS) {
    //         //     cons_pointer = val_unwrap_cons(interior);

    //         //     val_t forwarding_val = *((val_t*) cons_pointer);
    //         //     val_t* forwarding_pointer = (val_t*) val_unwrap_cons(forwarding_val);

    //         //     if (val_typeof(forwarding_val) == T_CONS && forwarding_pointer-to >= 0 && forwarding_pointer-to < heap_size) {
    //         //         *curr = forwarding_val;
    //         //     }
    //         //     else {
    //         //         // copy the cons from from_space to to_space
    //         //         val_cons_t* cons_heap = (val_cons_t*) rbx;
    //         //         *cons_heap = *cons_pointer;
                    
    //         //         // set curr to cons in to_space
    //         //         *curr = val_wrap_cons(cons_heap);

    //         //         // leave a forwarding pointer
    //         //         val_t* forwarding_ptr = (val_t*) cons_pointer;
    //         //         *forwarding_ptr = val_wrap_cons(cons_heap);

    //         //         // increment the rbx pointer
    //         //         cons_heap++;
    //         //         rbx = (val_t*) cons_heap;

    //         //         // enqueue the type
    //         //         types[end_queue] = T_CONS;
    //         //         end_queue++;
    //         //     }
    //         // }
    //         // else {
    //         //     ;
    //         // }

    //         val_box_t* curr_box = (val_box_t*) curr;
    //         curr_box++;
    //         curr = (val_t*) curr_box;
    //     }
    //     else if (type == T_CONS) {
    //         val_t first = ((val_cons_t*) curr)->fst;
    //         val_t second = ((val_cons_t*) curr)->snd;
    //         type_t first_type = val_typeof(first);
    //         type_t second_type = val_typeof(second);

    //         process_element(first, first_type, &(((val_cons_t*) curr)->fst), rbx, &rbx);
    //         process_element(second, second_type, &(((val_cons_t*) curr)->snd), rbx, &rbx);

    //         val_cons_t* curr_cons = (val_cons_t*) curr;
    //         curr_cons++;
    //         curr = (val_t*) curr_cons;
    //     }
    //     else if (type == T_VECT) {
    //         uint64_t length = ((val_vect_t*) curr)->len;

    //         for (int i = 0; i < length; ++i) {
    //             val_t interior = ((val_vect_t*) curr)->elems[i];
    //             type_t interior_type = val_typeof(interior);

    //             process_element(interior, interior_type, &(((val_vect_t*) curr)->elems[i]), rbx, &rbx);
    //         }

    //         curr += (1+length);
    //     }
    //     else if (type == T_STR) {
    //         uint64_t length = ((val_str_t*) curr)->len;
    //         length += (length % 2);

    //         curr += (1+length/2);
    //     }
    // }

    // printf("\nAfter Garbage Collection\nStack:\n");
    // print_memory(old_rsp, rbp, "stack");
    // printf("From Space:\n");
    // print_memory(from, old_rbx, "heap");
    // printf("To Space:\n");
    // print_memory(to, rbx, "heap");

    val_t* temp = to;
    to = from;
    from = temp;
    
    // printf("\nrbx: %p\n", rbx);
    // printf("to space: %p %ld\n", to, to);
    // printf("from space: %p %ld\n\n", from, from);

    return rbx;
}


void process_element(val_t interior, type_t interior_type, val_t* curr, val_t* rbx, val_t** rbx_double) {
    // printf("\nIn process_element\n");
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

            // push ptr to box into stk
            stk[top_stk] = rbx;
            top_stk++;

            // increment the rbx pointer
            box_heap++;
            *rbx_double = (val_t*) box_heap;

            // enqueue the type
            // types[end_queue] = T_BOX;
            // end_queue++;
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

            // push ptr to fst and snd into stk
            stk[top_stk] = &(cons_heap->fst);
            top_stk++;
            stk[top_stk] = &(cons_heap->snd);
            top_stk++;

            // increment the rbx pointer
            cons_heap++;
            *rbx_double = (val_t*) cons_heap;

            // enqueue the type
            // types[end_queue] = T_CONS;
            // end_queue++;
        }
    }
    else if (interior_type == T_VECT) {
        vect_pointer = val_unwrap_vect(interior);

        val_t forwarding_val = *((val_t*) vect_pointer);
        val_t* forwarding_pointer = (val_t*) val_unwrap_vect(forwarding_val);
        if (val_typeof(forwarding_val) == T_VECT && forwarding_pointer - to >= 0 && forwarding_pointer - to < heap_size) {
            *curr = forwarding_val;
        }
        else {
            // copy the vect from from_space to to_space
            val_vect_t* vect_heap = (val_vect_t*) rbx;
            vect_heap->len = vect_pointer->len;
            uint64_t len = vect_pointer->len;
            for (int i = 0; i < len; ++i) {
                vect_heap->elems[i] = vect_pointer->elems[i];
            }

            // set curr to vect in to_space
            *curr = val_wrap_vect(vect_heap);

            // leave a forwarding pointer
            val_t* forwarding_ptr = (val_t*) vect_pointer;
            *forwarding_ptr = val_wrap_vect(vect_heap);

            // push ptr to each elem into stk
            for (int i = 0; i < len; ++i) {
                stk[top_stk] = &(vect_heap->elems[i]);
                top_stk++;
            }

            // increment the rbx pointer
            *rbx_double += (1+len);

            // enqueue the type
            // types[end_queue] = T_VECT;
            // end_queue++;
        }
    }
    else if (interior_type == T_STR) {
        str_pointer = val_unwrap_str(interior);

        val_t forwarding_val = *((val_t*) str_pointer);
        val_t* forwarding_pointer = (val_t*) val_unwrap_str(forwarding_val);
        if (val_typeof(forwarding_val) == T_STR && forwarding_pointer - to >= 0 && forwarding_pointer - to < heap_size) {
            *curr = forwarding_val;
        }
        else {
            // copy the str from from_space to to_space
            val_str_t* str_heap = (val_str_t*) rbx;
            str_heap->len = str_pointer->len;
            uint64_t len = str_pointer->len;
            len += (len % 2);
            for (int i = 0; i < len; ++i) {
                str_heap->codepoints[i] = str_pointer->codepoints[i];
            }

            // set curr to str in to_space
            *curr = val_wrap_str(str_heap);

            // leave a forwarding pointer
            val_t* forwarding_ptr = (val_t*) str_pointer;
            *forwarding_ptr = val_wrap_str(str_heap);

            // increment the rbx pointer
            *rbx_double += (1+len/2);

            // enqueue the type
            types[end_queue] = T_STR;
            end_queue++;
        }
    }
    else if (interior_type == T_PROC) {
        val_t* proc_pointer = (val_t *)(interior ^ proc_type_tag);

        val_t forwarding_val = *proc_pointer;
        val_t* forwarding_pointer = (val_t *)(forwarding_val ^ proc_type_tag);
        if (val_typeof(forwarding_val) == T_PROC && forwarding_pointer - to >= 0 && forwarding_pointer - to < heap_size) {
            *curr = forwarding_val;
        }
        else {
            // get # of arguments
            val_t* proc_loc = (val_t*) (*proc_pointer);
            uint64_t len = (uint64_t) *(proc_loc-1);

            // copy proc location to to_space
            *rbx = *proc_pointer;

            // set curr to proc in to_space
            *curr = ((val_t) rbx) | proc_type_tag;

            // leave a forwarding pointer
            *proc_pointer = ((val_t) rbx) | proc_type_tag;

            // copy the arguments
            rbx++;
            proc_pointer++;
            for (int i = 0; i < len; ++i) {
                *rbx = *proc_pointer;

                stk[top_stk] = rbx;
                top_stk++;

                ++rbx;
                ++proc_pointer;
            }

            // increment rbx_double
            *(rbx_double) += (1+len);

            // enqueue the type
            types[end_queue] = T_PROC;
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
            // printf("\tElems Addr: %p", vect_pointer->elems);
        }
        else if (type == T_STR) {
            str_pointer = val_unwrap_str(val);
            printf("\tPointer: %p", str_pointer);

            // printf("\n");
            // uint64_t length = str_pointer->len;
            // for (int i = 0; i < length+1; ++i) {
            //     val_char_t ch = str_pointer->codepoints[i];
            //     printf("%s: %p\tVal: %x\tType: %d", mem, &(str_pointer->codepoints[i]), ch, T_CHAR);
            //     printf("\tInterior: ");
            //     print_str_char(ch);
            //     printf("\n");
            // }
        }
        // else if (type == T_PROC) {
        //     val_t* proc_pointer = (val_t *)(val ^ proc_type_tag);
        //     printf("\tPointer: %p", proc_pointer);
        //     val_t* proc = (val_t*) (*proc_pointer);
        //     proc--;
        //     printf("\tCount: %lx", *proc);
        // }

        // printf("\tInterior: ");
        // print_result(val);
        printf("\n");
        ++base;
    }
}