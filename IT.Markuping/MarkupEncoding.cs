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

    public bool IsStrict => _abc.Length == 7;

    /// <summary>
    /// &lt;
    /// </summary>
    public ReadOnlySpan<T> Lt => _abc.AsSpan(0, _size);

    /// <summary>
    /// &gt;
    /// </summary>
    public ReadOnlySpan<T> Gt => _abc.AsSpan(0, _size);

    /// <summary>
    /// /
    /// </summary>
    public ReadOnlySpan<T> Slash => _abc.AsSpan(0, _size);

    /// <summary>
    /// :
    /// </summary>
    public ReadOnlySpan<T> Colon => _abc.AsSpan(0, _size);

    /// <summary>
    /// ' '
    /// </summary>
    public ReadOnlySpan<T> Space => _abc.AsSpan(0, _size);

    /// <summary>
    /// "
    /// </summary>
    public ReadOnlySpan<T> Quot => _abc.AsSpan(0, _size);

    /// <summary>
    /// '
    /// </summary>
    public ReadOnlySpan<T> Apos => _abc.AsSpan(0, _size);

    /// <summary>
    /// =
    /// </summary>
    public ReadOnlySpan<T> Eq => _abc.AsSpan(0, _size);
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
    /// \r
    /// </summary>
    public ReadOnlySpan<T> CR => _abc.AsSpan(0, _size);

    /// <summary>
    /// \n
    /// </summary>
    public ReadOnlySpan<T> LF => _abc.AsSpan(0, _size);

    /// <summary>
    /// \t
    /// </summary>
    public ReadOnlySpan<T> Tab => _abc.AsSpan(0, _size);

    //public bool HasOtherSpaces => !_cr.Equals(default) && !_lf.Equals(default) && !_tab.Equals(default);

    #endregion Props

    public MarkupEncoding(int size, T[] abc)
    {
        if (size < 1) throw new ArgumentOutOfRangeException(nameof(size));
        if (abc == null) throw new ArgumentNullException(nameof(abc));
        if (abc.Length != 11 * size) throw new ArgumentNullException(nameof(abc));

        _size = size;
        _abc = abc;
    }

    public static explicit operator MarkupEncodingTokens<T>(MarkupEncoding<T> encoding)
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

        throw new InvalidCastException();
    }
}