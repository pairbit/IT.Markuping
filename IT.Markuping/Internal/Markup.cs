using System;

namespace IT.Markuping;

internal readonly ref struct Markup<T> where T : unmanaged
{
    internal readonly ReadOnlySpan<T> _data;
    internal readonly ReadOnlySpan<int> _ranges;
    internal readonly int _offset;

    public Markup(ReadOnlySpan<T> data, ReadOnlySpan<int> ranges, int offset = 0)
    {
        _data = data;
        _ranges = ranges;
        _offset = offset;
    }
}