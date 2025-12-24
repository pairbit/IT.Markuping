namespace IT.Markuping;

//<>/: "'=!-[]?\r\n\t
public readonly struct MarkupEncodingTokens<T> where T : unmanaged//, IEquatable<T>
{
    #region Fields

    internal readonly T _lt;//<
    internal readonly T _gt;//>
    internal readonly T _slash;//'/'
    internal readonly T _colon;//:
    internal readonly T _space;//" "

    //атрибуты
    internal readonly T _quot;//"
    internal readonly T _eq;//=

    /*
    //comments <!-- -->
    internal readonly T _excl;//! exclamation mark
    internal readonly T _dash;//-
    //<![CDATA[]]>
    internal readonly T _lsqb;//[ Left square bracket
    internal readonly T _rsqb;//] Right square bracket
    //декларация <? ?>
    internal readonly T _quest;//?
    */

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
    /*
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
    */

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

    //public bool HasOtherSpaces => !_cr.Equals(default) && !_lf.Equals(default) && !_tab.Equals(default);

    #endregion Props

    public MarkupEncodingTokens(T lt, T gt, T slash, T colon, T space, T quot, T eq,
        /*T excl, T dash, T lsqb, T rsqb, T quest,*/ T apos, T cr, T lf, T tab)
    {
        _lt = lt;
        _gt = gt;
        _slash = slash;
        _colon = colon;
        _space = space;
        _quot = quot;
        _eq = eq;/*
        _excl = excl;
        _dash = dash;
        _lsqb = lsqb;
        _rsqb = rsqb;
        _quest = quest;*/
        _apos = apos;
        _cr = cr;
        _lf = lf;
        _tab = tab;
    }
}