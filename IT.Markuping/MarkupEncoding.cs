using System;
using System.Runtime.CompilerServices;

namespace IT.Markuping;

public readonly struct MarkupEncoding<T> where T : unmanaged
{
    private readonly T[] _abc;
    private readonly int _size;

    #region Props

    public ReadOnlySpan<T> Abc => _abc;

    public int Size => _size;

    public bool IsComplex => _size > 1;

    public bool IsStrict => _abc.Length == 7 * _size;

    /// <summary>
    /// &lt;
    /// </summary>
    public ReadOnlySpan<T> LT => GetByIndex(0);

    /// <summary>
    /// &gt;
    /// </summary>
    public ReadOnlySpan<T> GT => GetByIndex(1);

    /// <summary>
    /// /
    /// </summary>
    public ReadOnlySpan<T> Slash => GetByIndex(2);

    /// <summary>
    /// :
    /// </summary>
    public ReadOnlySpan<T> Colon => GetByIndex(3);

    /// <summary>
    /// ' '
    /// </summary>
    public ReadOnlySpan<T> Space => GetByIndex(4);

    /// <summary>
    /// "
    /// </summary>
    public ReadOnlySpan<T> Quot => GetByIndex(5);

    /// <summary>
    /// =
    /// </summary>
    public ReadOnlySpan<T> Eq => GetByIndex(6);
    /*
    /// <summary>
    /// !
    /// </summary>
    public ReadOnlySpan<T> Excl => _abc.AsSpan(0, _size);

    /// <summary>
    /// -
    /// </summary>
    public ReadOnlySpan<T> Dash => _abc.AsSpan(0, _size);

    /// <summary>
    /// [
    /// </summary>
    public ReadOnlySpan<T> LSQB => _abc.AsSpan(0, _size);

    /// <summary>
    /// ]
    /// </summary>
    public ReadOnlySpan<T> RSQB => _abc.AsSpan(0, _size);

    /// <summary>
    /// ?
    /// </summary>
    public ReadOnlySpan<T> Quest => _abc.AsSpan(0, _size);
    */

    /// <summary>
    /// '
    /// </summary>
    public ReadOnlySpan<T> Apos => GetByIndex(7);

    /// <summary>
    /// \r
    /// </summary>
    public ReadOnlySpan<T> CR => GetByIndex(8);

    /// <summary>
    /// \n
    /// </summary>
    public ReadOnlySpan<T> LF => GetByIndex(9);

    /// <summary>
    /// \t
    /// </summary>
    public ReadOnlySpan<T> Tab => GetByIndex(10);

    //public bool HasOtherSpaces => !_cr.Equals(default) && !_lf.Equals(default) && !_tab.Equals(default);

    #endregion Props

    public MarkupEncoding(int size, T[] abc)
    {
        if (size < 1) throw new ArgumentOutOfRangeException(nameof(size));
        if (abc == null) throw new ArgumentNullException(nameof(abc));
        var length = abc.Length;
        if (length != 7 * size && length != 11 * size) throw new ArgumentOutOfRangeException(nameof(abc));

        _size = size;
        _abc = abc;
    }

    private ReadOnlySpan<T> GetByIndex(int index)
    {
        var start = _size * index;
        if (_abc.Length < start + _size) return default;

        return _abc.AsSpan(start, _size);
    }

    public static explicit operator MarkupEncodingTokens<T>(MarkupEncoding<T> encoding)
    {
        if (!encoding.IsComplex)
        {
            var abc = encoding._abc;
            if (abc.Length == 7)
            {
                return new(abc[0], abc[1], abc[2], abc[3], abc[4], abc[5], abc[6], default, default, default, default);
            }
            if (abc.Length == Unsafe.SizeOf<MarkupEncodingTokens<T>>())
            {
                return Unsafe.As<T, MarkupEncodingTokens<T>>(ref abc[0]);
            }
        }
        throw new InvalidCastException();
    }
}