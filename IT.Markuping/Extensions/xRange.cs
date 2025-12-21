using System;

namespace IT.Markuping.Extensions;

internal static class xRange
{
    public static bool IsZero(this Range range) => range.Start.Value == 0 && range.End.Value == 0;

    public static Range AddOffset(this Range range, int offset)
        => offset == 0 ? range : new(offset + range.Start.Value, offset + range.End.Value);
}