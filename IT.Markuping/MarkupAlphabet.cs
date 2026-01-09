using System;
using System.Runtime.CompilerServices;

namespace IT.Markuping;

public readonly struct MarkupAlphabet<T> where T : unmanaged
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
    public ReadOnlySpan<T> Excl => GetByIndex(7);

    /// <summary>
    /// -
    /// </summary>
    public ReadOnlySpan<T> Dash => GetByIndex(8);

    /// <summary>
    /// [
    /// </summary>
    public ReadOnlySpan<T> LSQB => GetByIndex(9);

    /// <summary>
    /// ]
    /// </summary>
    public ReadOnlySpan<T> RSQB => GetByIndex(10);

    /// <summary>
    /// ?
    /// </summary>
    public ReadOnlySpan<T> Quest => GetByIndex(11);

    public ReadOnlySpan<T> X => GetByIndex(12);
    public ReadOnlySpan<T> M => GetByIndex(13);
    public ReadOnlySpan<T> L => GetByIndex(14);
    public ReadOnlySpan<T> N => GetByIndex(15);
    public ReadOnlySpan<T> S => GetByIndex(16);
    public ReadOnlySpan<T> I => GetByIndex(17);
    public ReadOnlySpan<T> D => GetByIndex(18);
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

    public MarkupAlphabet(int size, T[] abc)
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

    public static explicit operator MarkupTokens<T>(MarkupAlphabet<T> alphabet)
    {
        if (!alphabet.IsComplex)
        {
            var abc = alphabet._abc;
            if (abc.Length == 7)
            {
                return new(abc[0], abc[1], abc[2], abc[3], abc[4], abc[5], abc[6], default, default, default, default);
            }
            if (abc.Length * Unsafe.SizeOf<T>() == Unsafe.SizeOf<MarkupTokens<T>>())
            {
                return Unsafe.As<T, MarkupTokens<T>>(ref abc[0]);
            }
        }
        throw new InvalidCastException();
    }
}