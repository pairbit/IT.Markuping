using System;

namespace IT.Markuping.Interfaces;

internal interface IMarkupEncoding<T> where T : unmanaged
{
    ReadOnlySpan<T> Lt { get; }
    ReadOnlySpan<T> Gt { get; }
    ReadOnlySpan<T> Slash { get; }
    ReadOnlySpan<T> Colon { get; }
    ReadOnlySpan<T> Space { get; }
    ReadOnlySpan<T> Quot { get; }
    ReadOnlySpan<T> Apos { get; }
    ReadOnlySpan<T> Eq { get; }
}