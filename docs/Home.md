# Welcome to the Mythix ORM wiki

## Getting started

Here you will not only find documentation for the Mythix ORM API, but also articles (in the "Pages" section) that explain in detail the inner workings of Mythix ORM.

To start with, we highly recommend reading the following articles:

  1. [Queries](./Queries)
  2. [Associations](./Associations)
  3. [Models](./Models)
  4. [Types](./TypesReference)
  5. [Connection Binding](./ConnectionBinding)

These should give you a basic idea on how to work with Mythix ORM.

After you have read these articles and understand what you are working with, take a look at the API documentation to understand _what_ you have available to you, and how you can use it.

## What Mythix ORM **is**

Mythix ORM has been designed to fix the "situation" found today with other ORMs for Node. Namely, it focuses on being bloat-free, having a clean usable interface, a simple to use yet advanced query interface, no hidden auto-magic, clean code, and finally, fantastic documentation. These are all points that are shockingly lacking, or extremely poorly handled in other ORMs currently available for Node, and I wanted to do something about it.

Mythix ORM is **not** a batteries-included, do everything-under-the-sun type of library. Instead, the authors have deliberately chosen to make it as small, fast, and focused as possible, while remaining extremely extensible. It is the hope of the authors that the community will build new features onto Mythix ORM via community supported third-party modules. The reasons for this are 1) Mythix ORM doesn't want to be bloat, and instead would like developers to be able to pick and choose which features they want to use, and 2) We feel it is better if the community decides the path forward. We will support and cheer the community on, and possibly even backport community modules into the core Mythix ORM feature set if there is widely adopted community support and demand for them.

Mythix ORM has been designed to be modified and added onto. There are no private or global variables used in Mythix ORM by design. It is the intent of the authors that you will modify the library for your specific use case, add onto it as you see fit, or completely change how it works to meet your needs. We don't play into the fallacy that we should lock everything down just so you don't shoot yourself in the foot. Instead, we open everything up to be modified, and if you shoot yourself in the foot, well, that is on you. We would rather enable you, as the developer, to do what you need to do to get your job done, instead of restricting what you can do for fear that you might cause yourself trouble. In short, hack away! Mythix ORM should serve you and your needs, not the other way around.

Mythix ORM intends to conquer the world through a modular design. Though we deliberately avoid bloat, and therefor don't have every possible feature baked into the core libraries, we do hope to provide--and hope the community will provide--a large selection of useful modules and plugins for Mythix technologies. It is our hope that in the coming years Mythix ORM will be able to interact with every database on the planet, and have many other useful libraries for developers to use. Feel free to contribute! We would love to see your custom database driver, how you have added onto and improved the query interface, or other improvements you come up with.

Mythix ORM deliberately tries to abstract everything it can away from the database. Because of this, you will often find cases where you might need to do things differently then you are used to, or you might find some of our design decisions a little strange. ORMs are *supposed* to be an abstraction layer, so it makes me shudder when I see other ORMs recommending database specific code, or providing database specific interfaces. Obviously there are times where this can not be avoided, and Mythix ORM does its best to handle these cases in an abstract way. However, there may be times where you just need to make a direct query to do something with your database, use a custom literal, or modify how Mythix ORM works by overloading its internal methods. One area that you will immediately notice this abstraction is the field types. There is a shockingly small number of field types available in Mythix ORM, and this is deliberate. Mythix ORM will *never* supply database specific field types... if you need those, you can use literals, or you can define your own field types to suit your needs. The situation also isn't as bad as you might initially think. Take the `INTEGER` type for example. It is designed such that it can receive as an optional parameter the "number of bytes" needed to store a certain integer type--and then it is up to the specific connection you are using to decide how to implement said type. For example, Mythix ORM does not supply the MySQL specific types like `TINYINT`, `SMALLINT`, or `MEDIUMINT`. Instead, you always simply use `INTEGER`, and specify the number of bytes you need, and the MySQL connection will take care of the rest for you. Field types are just one example. Mythix ORM will abstract away everything it can by deliberate design.

## What Mythix ORM **is not**

Mythix ORM **is not** bloated, and it never intends to be. We want to keep our library focused on a single task, and we want it to do that task well. We plan to rely on the community and support from third-party modules to add to the feature set of the library. All optional features (such as database drivers) will remain modules, enabling the developer to pick and choose only what they need.

Mythix ORM **is not** magical. It won't ever have built-in support for standard fields, such as `created_at`, or `updated_at`, it won't add fields to your tables behind the scenes, it won't mangle names to try and fit someone else's convention, and it won't hide interfaces or features via some stupid and pointless "black box" principle. What You See Is What You Get is a core design principle of ours. Without the magic we have accounted for what developers need. For example, if you need a `created_at` and `updated_at` field on every model of yours, simply create a base model with these fields, and then have all your other models inherit from this base model.

Mythix ORM **is not** opinionated--unless you consider the desire for clean code, simple interfaces, and fantastic documentation opinionated. We do not inform the developer for example that we have no `toSQL` method, and won't be adding one, "because developers should never use such a thing". Instead, we side with Journey, and openly state "anyway you want it is the way you need it". As a community we are here to support developers, not tell them how to live their lives.

## Mythix certifications

Mythix, as a community of libraries, has a certification program. We will certify third party libraries, and we recommend that you select certified libraries first. We will also maintain a blacklist of libraries that we feel aren't fitting to be used, or that damage our name. We have seen far too many examples of smelly, hacked together, thoughtless code floating around in the open source community, and would prefer our community doesn't devolve to such "standards". Indeed, Mythix ORM was born because of this problem, and we would like to push the engineering community in a positive direction, and not continue supporting the devolving of good engineering practices. We take pride in our name and what we do, and would like to be in a spot where people are confident and excited when they see the "Mythix" brand... not horrified and disappointed.

### How can I get my Mythix library certified?

You can get your Mythix library certified by opening a pull request against this wiki, adding your library to the list of certifications. This will notify our team that you wish to have your library certified. Our team will then review your entire library, your design decisions, and the quality of your code and your documentation. We may request changes to meet our standards. When your library meets our standards, we will accept your pull request, and your library will then be certified. By using Mythix libraries you accept and understand that Mythix ORM reserves the right to certify or blacklist any library at any time, for any reason.

Our standards focus on 1) The quality of your code in general, 2) How many dependencies you use, the quality of those dependencies, and why you have chosen to use them, 3) The quality, correctness, and completeness of your documentation, and 4) The design decisions you made, why you made them, and how clean and simple your interface is, and 5) How well your code is covered by tests.

We expect prompt communication when we ask clarifying questions or request changes. If we don't receive prompt responses from you, we will simply close your PR, and you will need to restart the process.

### How can I get my Mythix library off the blacklist?

The Mythix team will regularly groom [NPM](https://npmjs.com/) for Mythix libraries published to the community. If any are found that don't meet our standards, we will immediately add that library to our blacklist and notify the author. If you would like your library removed from our blacklist, then you will need to contact our team, and notify them that you have resolved the issues, and that you are ready to have your library certified. If for whatever reason you feel we are being unfair, then please open an issue on github under the `mythix-orm` repository describing your concerns. At this point, we, along with the community, will review your concerns, and decide how to proceed.

### Certification list

Our list of certified and blacklisted libraries can be found on our [Certifications](./Certifications) page.