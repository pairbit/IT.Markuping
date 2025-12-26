using System;
using System.Diagnostics;

namespace IT.Markuping.Internal;

internal static class xRange
{
    public static bool IsZero(this Range range) =>
        range.Equals(default);

    public static Range MultipleOffset(this Range range, int offset)
    {
        Debug.Assert(!range.Start.IsFromEnd && !range.End.IsFromEnd);
        return new(checked(range.Start.Value * offset), checked(range.End.Value * offset));
    }

    public static Range AddOffset(this Range range, int offset)
    {
        Debug.Assert(!range.Start.IsFromEnd && !range.End.IsFromEnd);
        return new(checked(offset + range.Start.Value), checked(offset + range.End.Value));
    }

    public static Range AddOffsetIfNotZero(this Range range, int offset)
    {
        return range.Equals(default) ? range : range.AddOffset(offset);
    }
}