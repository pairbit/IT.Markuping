using IT.Markuping.Interfaces;
using System;
using System.Buffers;

namespace IT.Markuping.Implementation;

internal class HtmlMarkuper : IMarkuper<byte>, IMarkuper<char>
{
    public Markup<byte> NewMarkup(ReadOnlySpan<byte> data, IBufferWriter<int> buffer)
    {
        throw new NotImplementedException();
    }

    public Markup<char> NewMarkup(ReadOnlySpan<char> data, IBufferWriter<int> buffer)
    {
        throw new NotImplementedException();
    }
}