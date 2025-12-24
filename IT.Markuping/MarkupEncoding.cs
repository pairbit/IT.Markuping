using System;

namespace IT.Markuping;

public readonly struct MarkupEncoding<T> where T : unmanaged
{
    private readonly T[] _abc;
    private readonly int _size;

    public int Size => _size;

    public bool IsComplex => _size > 1;

    public ReadOnlySpan<T> Lt => _abc.AsSpan(0, _size);

    public ReadOnlySpan<T> Gt => _abc.AsSpan(0, _size);

    public MarkupEncoding(int size, T[] abc)
    {
        if (size < 1) throw new ArgumentOutOfRangeException(nameof(size));
        if (abc == null) throw new ArgumentNullException(nameof(abc));
        if (abc.Length != 16 * size) throw new ArgumentNullException(nameof(abc));

        _size = size;
        _abc = abc;
    }
}