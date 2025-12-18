using System;
using System.Buffers;

namespace IT.Markuping.Interfaces;

internal interface IMarkuper<T> where T : unmanaged
{
    Markup<T> NewMarkup(ReadOnlySpan<T> data, IBufferWriter<int> buffer);
}