using IT.Markuping.Encodings;
using System.Diagnostics.CodeAnalysis;

namespace IT.Markuping.Implementation;

public class ComplexTagFinderByte : ComplexTagFinder<byte>
{
    public static readonly ComplexTagFinderByte Utf16 = new(BytesEncoding.Utf16);
    public static readonly ComplexTagFinderByte Utf16BE = new(BytesEncoding.Utf16BE);
    public static readonly ComplexTagFinderByte Utf32 = new(BytesEncoding.Utf32);
    public static readonly ComplexTagFinderByte Utf32BE = new(BytesEncoding.Utf32BE);

    public ComplexTagFinderByte(BytesEncoding bytesEncoding)
        :base(bytesEncoding._minLength, bytesEncoding._lt, bytesEncoding._gt, 
            bytesEncoding._slash, bytesEncoding._colon, bytesEncoding._space,
            bytesEncoding._quot, bytesEncoding._apos, bytesEncoding._eq, 
            bytesEncoding._otherSpaces
            )
    {

    }

    public static bool TryGet(int codePage, [MaybeNullWhen(false)] out ComplexTagFinderByte tagFinder)
    {
        if (codePage == 1200)
        {
            tagFinder = Utf16;
            return true;
        }
        if (codePage == 1201)
        {
            tagFinder = Utf16BE;
            return true;
        }
        if (codePage == 12000)
        {
            tagFinder = Utf32;
            return true;
        }
        if (codePage == 12001)
        {
            tagFinder = Utf32BE;
            return true;
        }
        tagFinder = null;
        return false;
    }
}