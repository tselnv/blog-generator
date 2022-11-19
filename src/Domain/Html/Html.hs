module Domain.Html.Html ( render,
      Html,
      Structure,
      Title,
      TextHtml(..),
      WrapHtml(append_, html_, p_, h1_, ul_, ol_, li_) )
      where

import Domain.Html.HtmlInternal
    ( render,
      Html,
      Structure,
      Title,
      WrapHtml(append_, html_, p_, h1_, ul_, ol_, li_) )
import Domain.Html.HtmlTextInternal (TextHtml(..))
