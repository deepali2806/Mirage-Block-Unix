(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type 'a promise
    (** Type of promises *)
    val async : (unit -> 'a) -> 'a promise
    (** [async f] runs [f] concurrently *)
    val await : 'a promise -> 'a
    (** [await p] returns the result of the promise. *)
    val yield : unit -> unit
    (** yields control to another task *)
    val run   : (unit -> 'a) -> unit
    (** Runs the scheduler *)
    
    (* val run : ?engine:[`Select | `Libev] -> (unit -> unit) -> unit *)
(*val run : unit Lwt.t -> unit*)
