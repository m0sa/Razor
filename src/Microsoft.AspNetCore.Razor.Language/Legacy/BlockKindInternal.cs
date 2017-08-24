// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace Microsoft.AspNetCore.Razor.Language.Legacy
{
    internal enum BlockKindInternal
    {
        // Code
        Statement,
        Directive,
        Expression,

        // Markup
        Markup,
        Template,

        // Special
        Comment,
        Tag
    }
}