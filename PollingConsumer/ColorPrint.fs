// Copyright (c) 2017 Mark Seemann.
// Permission to reproduce or modify is granted for personal, educational use.
// No warranty implied.

// This module adopted from
// http://blogs.msdn.com/b/chrsmith/archive/2008/10/01/f-zen.aspx

module Ploeh.Samples.ColorPrint

open Microsoft.FSharp.Core.Printf

let cprintf color format =
    let print (s : string) = 
        let old = System.Console.ForegroundColor 
        try 
            System.Console.ForegroundColor <- color;
            System.Console.Write s
        finally
            System.Console.ForegroundColor <- old
    kprintf print format

let cprintfn color format = 
    cprintf color format
    printfn ""