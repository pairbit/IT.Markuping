using IT.Markuping.Internal;
using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace IT.Markuping;

public readonly struct MarkupAlphabet<T> where T : unmanaged
{
    private readonly T[] _abc;

    #region Props

    public ReadOnlySpan<T> Abc => _abc;

    public int Size => _abc.Length / MarkupAlphabet.Length;

    public bool IsComplex => _abc.Length != MarkupAlphabet.Length;

    //public bool IsStrict => _abc.Length == MarkupAlphabet.StrictLength * _size;

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

    public ReadOnlySpan<T> x => GetByIndex(12);

    public ReadOnlySpan<T> m => GetByIndex(13);

    public ReadOnlySpan<T> l => GetByIndex(14);

    public ReadOnlySpan<T> n => GetByIndex(15);

    public ReadOnlySpan<T> s => GetByIndex(16);

    public ReadOnlySpan<T> i => GetByIndex(17);

    public ReadOnlySpan<T> d => GetByIndex(18);

    public ReadOnlySpan<T> I => GetByIndex(19);

    public ReadOnlySpan<T> D => GetByIndex(20);

    /// <summary>
    /// '
    /// </summary>
    public ReadOnlySpan<T> Apos => GetByIndex(21);

    /// <summary>
    /// \r
    /// </summary>
    public ReadOnlySpan<T> CR => GetByIndex(22);

    /// <summary>
    /// \n
    /// </summary>
    public ReadOnlySpan<T> LF => GetByIndex(23);

    /// <summary>
    /// \t
    /// </summary>
    public ReadOnlySpan<T> Tab => GetByIndex(24);

    #endregion Props

    public MarkupAlphabet(T[] abc)
    {
        if (abc == null) throw new ArgumentNullException(nameof(abc));
        if (abc.Length % MarkupAlphabet.Length != 0)
            throw new ArgumentOutOfRangeException(nameof(abc));

        _abc = abc;
    }

    public bool Equals(MarkupAlphabet<T> other) => _abc == other._abc;

    private ReadOnlySpan<T> GetByIndex(int index)
    {
        var size = Size;
        var start = size * index;
        if (_abc.Length < start + size) return default;

        return _abc.AsSpan(start, size);
    }

    public static explicit operator MarkupTokens<T>(MarkupAlphabet<T> alphabet)
    {
        if (alphabet.IsComplex) throw new InvalidCastException("Alphabet is complex.");

        Debug.Assert(alphabet.Size == 1);

        Debug.Assert(alphabet._abc.Length * Unsafe.SizeOf<T>() == Unsafe.SizeOf<MarkupTokens<T>>());

        return Unsafe.As<T, MarkupTokens<T>>(ref alphabet._abc[0]);
    }
}