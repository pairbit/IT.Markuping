using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace IT.Markuping;

internal static class MarkupAlphabet
{
    public const int Length = 23;
    public const int StrictLength = Length - 4;
}

public readonly struct MarkupAlphabet<T> where T : unmanaged
{
    private readonly T[] _abc;
    private readonly int _size;

    #region Props

    public ReadOnlySpan<T> Abc => _abc;

    public int Size => _size;

    public bool IsComplex => _size > 1;

    public bool IsStrict => _abc.Length == MarkupAlphabet.StrictLength * _size;

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

    public ReadOnlySpan<T> X => GetByIndex(12);
    public ReadOnlySpan<T> M => GetByIndex(13);
    public ReadOnlySpan<T> L => GetByIndex(14);
    public ReadOnlySpan<T> N => GetByIndex(15);
    public ReadOnlySpan<T> S => GetByIndex(16);
    public ReadOnlySpan<T> I => GetByIndex(17);
    public ReadOnlySpan<T> D => GetByIndex(18);

    /// <summary>
    /// '
    /// </summary>
    public ReadOnlySpan<T> Apos => GetByIndex(19);

    /// <summary>
    /// \r
    /// </summary>
    public ReadOnlySpan<T> CR => GetByIndex(20);

    /// <summary>
    /// \n
    /// </summary>
    public ReadOnlySpan<T> LF => GetByIndex(21);

    /// <summary>
    /// \t
    /// </summary>
    public ReadOnlySpan<T> Tab => GetByIndex(22);

    #endregion Props

    public MarkupAlphabet(int size, T[] abc)
    {
        if (size < 1) throw new ArgumentOutOfRangeException(nameof(size));
        if (abc == null) throw new ArgumentNullException(nameof(abc));
        var length = abc.Length;
        if (length != MarkupAlphabet.StrictLength * size && 
            length != MarkupAlphabet.Length * size) 
            throw new ArgumentOutOfRangeException(nameof(abc));

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
            Debug.Assert(alphabet._size == 1);

            var abc = alphabet._abc;
            var length = abc.Length;
            if (length == MarkupAlphabet.StrictLength)
            {
                Span<T> full = stackalloc T[MarkupAlphabet.Length];
                abc.AsSpan().CopyTo(full);
                return Unsafe.As<T, MarkupTokens<T>>(ref full[0]);
            }
            if (length == MarkupAlphabet.Length)
            {
                Debug.Assert(abc.Length * Unsafe.SizeOf<T>() == Unsafe.SizeOf<MarkupTokens<T>>());

                return Unsafe.As<T, MarkupTokens<T>>(ref abc[0]);
            }
        }
        throw new InvalidCastException();
    }
}