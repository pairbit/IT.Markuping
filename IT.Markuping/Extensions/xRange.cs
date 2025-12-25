using System;

namespace IT.Markuping.Extensions;

internal static class xRange
{
    public static bool IsZero(this Range range) => range.Start.Value == 0 && range.End.Value == 0;

    public static Range MultipleOffset(this Range range, int offset)
        => new(checked(range.Start.Value * offset), checked(range.End.Value * offset));

    public static Range AddOffset(this Range range, int offset)
        => new(offset + range.Start.Value, offset + range.End.Value);

    public static Range AddOffsetIfNotZero(this Range range, int offset)
    {
        var start = range.Start.Value;
        var end = range.End.Value;
        return start == 0 && end == 0 ? range : new(start + offset, end + offset);
    }
}