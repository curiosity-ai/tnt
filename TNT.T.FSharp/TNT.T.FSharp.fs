﻿namespace TNT.T.FSharp

open System.Runtime.CompilerServices

module Extensions =

    type System.String with

        [<MethodImpl(MethodImplOptions.NoInlining)>]
        member this.t = this

[<assembly: AutoOpen("TNT.T.FSharp.Extensions")>]
do ()
