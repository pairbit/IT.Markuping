using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace IT.Markuping;

//<>/: "=!-[]?xmlnsid'\r\n\t
public readonly struct MarkupTokens<T> where T : unmanaged
{
    private const int Length = 23;

    #region Fields

    internal readonly T _lt;//<
    internal readonly T _gt;//>
    internal readonly T _slash;//'/'
    internal readonly T _colon;//:
    internal readonly T _space;//" "

    //атрибуты
    internal readonly T _quot;//"
    internal readonly T _eq;//=

    //comments <!-- -->
    internal readonly T _excl;//! exclamation mark
    internal readonly T _dash;//-
    //<![CDATA[]]>
    internal readonly T _lsqb;//[ Left square bracket
    internal readonly T _rsqb;//] Right square bracket
    //декларация <? ?>
    internal readonly T _quest;//?
    internal readonly T _x;
    internal readonly T _m;
    internal readonly T _l;
    internal readonly T _n;
    internal readonly T _s;
    internal readonly T _i;
    internal readonly T _d;

    //not strict
    internal readonly T _apos;//'

    //other spaces
    internal readonly T _cr;//\r
    internal readonly T _lf;//\n
    internal readonly T _tab;//\t

    #endregion Fields

    #region Props

    /// <summary>
    /// &lt;
    /// </summary>
    public T LT => _lt;

    /// <summary>
    /// &gt;
    /// </summary>
    public T GT => _gt;

    /// <summary>
    /// /
    /// </summary>
    public T Slash => _slash;

    /// <summary>
    /// :
    /// </summary>
    public T Colon => _colon;

    /// <summary>
    /// ' '
    /// </summary>
    public T Space => _space;

    /// <summary>
    /// "
    /// </summary>
    public T Quot => _quot;

    /// <summary>
    /// =
    /// </summary>
    public T Eq => _eq;
    
    /// <summary>
    /// !
    /// </summary>
    public T Excl => _excl;

    /// <summary>
    /// -
    /// </summary>
    public T Dash => _dash;

    /// <summary>
    /// [
    /// </summary>
    public T LSQB => _lsqb;

    /// <summary>
    /// ]
    /// </summary>
    public T RSQB => _rsqb;

    /// <summary>
    /// ?
    /// </summary>
    public T Quest => _quest;

    public T X => _x;
    public T M => _m;
    public T L => _l;
    public T N => _n;
    public T S => _s;
    public T I => _i;
    public T D => _d;

    /// <summary>
    /// '
    /// </summary>
    public T Apos => _apos;

    /// <summary>
    /// \r
    /// </summary>
    public T CR => _cr;

    /// <summary>
    /// \n
    /// </summary>
    public T LF => _lf;

    /// <summary>
    /// \t
    /// </summary>
    public T Tab => _tab;

    #endregion Props

    public MarkupTokens(ReadOnlySpan<T> span)
    {
        if (span.Length != Length) throw new ArgumentOutOfRangeException(nameof(span));
        this = Unsafe.As<T, MarkupTokens<T>>(ref MemoryMarshal.GetReference(span));
    }

    public MarkupTokens(T lt, T gt, T slash, T colon, T space, T quot, T eq,
        T excl, T dash, T lsqb, T rsqb, T quest,
        T x, T m, T l, T n, T s, T i, T d,
        T apos, T cr, T lf, T tab)
    {
        _lt = lt;
        _gt = gt;
        _slash = slash;
        _colon = colon;
        _space = space;
        _quot = quot;
        _eq = eq;
        _excl = excl;
        _dash = dash;
        _lsqb = lsqb;
        _rsqb = rsqb;
        _quest = quest;
        _x = x;
        _m = m;
        _l = l;
        _n = n;
        _s = s;
        _i = i;
        _d = d;
        _apos = apos;
        _cr = cr;
        _lf = lf;
        _tab = tab;
    }
}