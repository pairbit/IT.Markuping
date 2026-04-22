using IT.Markuping.Interfaces;
using IT.Markuping.Internal;
using System;
using System.Runtime.InteropServices;

namespace IT.Markuping.Tests;

internal class IdEquatabe<T> : IAttName where T : unmanaged, IEquatable<T>
{
    private readonly T _colon;
    private readonly T _I, _i, _d, _D;

    public IdEquatabe(MarkupAlphabet<T> alphabet)
    {
        var token = (MarkupTokens<T>)alphabet;
        _colon = token.Colon;
        _i = token.i;
        _I = token.I;
        _d = token.d;
        _D = token.D;
    }

    bool IAttName.Equals<T2>(ReadOnlySpan<T2> tagName, ReadOnlySpan<T2> attName, ReadOnlySpan<T2> dtd)
    {
        if (typeof(T) == typeof(T2)) return Equals(MemoryMarshal.Cast<T2, T>(tagName), MemoryMarshal.Cast<T2, T>(attName), MemoryMarshal.Cast<T2, T>(dtd));

        throw new NotImplementedException();
    }

    //id, Id, ID
    public bool Equals(ReadOnlySpan<T> tagName, ReadOnlySpan<T> attName, ReadOnlySpan<T> dtd)
    {
#if DEBUG && NET
        var tagNameStr = Info.ToString(tagName);
        var attNameStr = Info.ToString(attName);
        var dtdStr = Info.ToString(dtd);
#endif

        var index = attName.IndexOf(_colon);
        if (index > -1)
        {
            attName = attName.Slice(index + 1);
        }

        if (attName.Length == 2)
        {
            var first = attName[0];
            if (first.Equals(_i))
                return attName[1].Equals(_d);

            if (first.Equals(_I))
            {
                var second = attName[1];
                return second.Equals(_d) || second.Equals(_D);
            }
        }

        return false;
    }
}