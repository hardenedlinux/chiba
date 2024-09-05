# Chiba project - the framework for next generation of cloud computing

[日本語](docs/README_jp.md)

## Intro

You might hear about next-gen data centers, or as they're more commonly known, [containerized data centers](https://www.techtarget.com/searchdatacenter/tip/An-overview-of-containerized-data-centers-and-their-benefits). The container mentioned here doesn't refer to a Docker container, but rather the actual shipping container you'd see at a port, typically carried on a truck trailer.

<center>
<img src="https://www.deltapowersolutions.com/media/images/products/datacenter-solutions-Containerized-datacenter-truck-inside.gif" alt="containerized data center"/>
</center>

<center>
<img src="https://hardenedvault.net/images/blog/data-center-evo_hu088243e27fca90948e44192083ac0422_130352_1713x940_fit_q100_h2_box_3.webp" alt="data center evolution"/>
</center>

### How it works?

<center>
<img src="pub/img/chiba-works.png" alt="How Chiba works"/>
</center>

The containerized data center is a modular data center that is tailorable, portable and can be transported to any location. It's a self-contained unit that includes all the necessary components for a data center, such as servers, storage, networking equipment, and cooling systems. The containerized data center is typically housed in a shipping container, which makes it easy to transport and deploy.

Chiba project is a framework that leverages OpenBMC to provide infrastructure automation. It maintains backward compatibility with traditional cloud computing requirements like Kubernetes and virtualization. The Chiba project is designed to be a flexible and extensible framework that can be easily customized to meet the specific needs of different organizations.

Typically, Chiba runs on a management server that communicates with the OpenBMC firmware on the servers in the containerized data center. Chiba will interpreter the SCL (Server Configuration Language) scripts to manage the servers in the containerized data center. The SCL scripts can be used to define the configuration of the servers, monitor the servers, and take action based on the monitoring results. The SCL scripts will be executed by the Chiba server and finally transformed to Redfish API sent to the OpenBMC firmware of all servers in the containerized data center.

### Why name Chiba

Chiba is a Japan city with port, containers, and dockers. In [Neuromancer](https://en.wikipedia.org/wiki/Neuromancer), it's also named **night city**. the underworld of the hackers that monitors and controls the upper world. So if you like the **night city** in 2077, now you know where it is.

The core concept of Chiba project is to take advantage of [OpenBMC](https://en.wikipedia.org/wiki/OpenBMC), the firmware that monitors and controls the upper world upon the motherboard, or you may say, the virtual reality dataspace that was named **the matrix** in Neuromancer.

## Why make Chiba?

Chiba project was created for fun and for profit.

Nowadays, we're actually facing an extreamly complex supply chain. There are several neo cutting-edge models to cope with the complexity:
- **Mainframe-Like all in one solution**: It is using the mainframe methodology to build new supply chain for x86 servers. E.g, [Oxide](https://oxide.computer/).
- **GPU-oriented computing node**: To maximize the performance of a single node while playing the role of an ODM, ensuring the best cost-performance ratio. E.g, [Tinycorp - Tinybox](https://tinygrad.org/#tinybox).
- **Building block approach**: Abstract the critical components in the OEM supply chain into building blocks that can be freely combined based on the needs of industry users. E.g, [HardenedVault](https://hardenedvault.net/).

### There're projects for controling OpenBMC, why bother to create another one?

Yes, there're projects for OpenBMC and Redfish API.

However, Chiba project is not for single node OpenBMC, it's for many or even massive OpenBMC nodes. It's for the next generation of cloud computing infrastructure.

### Who's behind Chiba?

Chiba project has been developing by [HardenedLinux](https://hardenedlinux.org), which was entitled by [Open Source Firmware Conference](https://osfc.io). OSFC is the organization devoted to building a community around Open Ecosystem of supplychain.

<center>
<img src="https://www.osfc.io/assets/images/projects/hardened-linux.svg" alt="HardenedLinux" width=400 />
</center>

### Who should use Chiba?

- **Cloud service providers**: Chiba can be used to build the next generation of cloud computing infrastructure.
- **Enterprises**: Chiba can be used to build the private cloud infrastructure.
- **Hardware vendors**: Chiba can be used to provide the firmware enhancement for the hardware products.
- **Security vendors**: Chiba can be used to provide the firmware security enhancement for the hardware products.

For now, we don't have real hardware (rack with OpenBMC) to test. If you are any of the above roles and interested in Chiba, please contact us: NalaGinrut@hardenedlinux.org.

**And if you are happen in Tokyo, let's have a coffee ;-).**

For technical geeks, Chiba is a good place to learn the OpenBMC and security enhancement for next generation of cloud computing.

## What's special?

### Infrastructure automation - the key point of the next-gen data centers

The traditional cloud computing is based on the virtualization, which is a software layer that abstracts the hardware resources. In the coming AI era, the hardware resources may include dedicated hardware accelerators, such as GPU, TPU, FPGA, etc. The virtualization is not enough to manage these resources. The auto scaling, auto healing, auto upgrading, etc, are the key features of the next generation of cloud computing. All these features are based on the core firmware of the hardware, which is the OpenBMC.

The Chiba project is a framework that leverages OpenBMC to provide infrastructure automation. It maintains backward compatibility with traditional cloud computing requirements like Kubernetes and virtualization.

### Security enhancement - prevent the Maginot Line

You may have good security software, such as firewall, IDS, IPS, etc. Linux kernel enhancement, such as SELinux, AppArmor. Or even advanced Linux kernel vaccine like [Grsecurity](https://grsecurity.net/), [VED from HardenedVault](https://hardenedvault.net/blog/2021-09-06-ved/), or [AKO from Hitachi](https://link.springer.com/content/pdf/10.1007/s10207-020-00514-7.pdf)...try to imagine, what if the attacker has the root privilege of the firmware like OpenBMC?

Then eveything is over. All your cybersecurity efforts become stupid [Maginot Line](https://en.wikipedia.org/wiki/Maginot_Line).

<center>
<img src="https://hardenedvault.net/images/blog/fw-threat-model_hu68b998ae0ef17d5fdf9fc904d373af2d_66679_537x788_fit_q100_h2_box_3.webp" alt="firmware attack"/>
</center>

The only viable solution is to enhance security at the most fundamental level: the firmware. The Chiba project requires a securely enhanced OpenBMC firmware. While standard APIs are used, the actual OpenBMC firmware is typically customized by hardware vendors. This customization is necessary due to variations in resource definitions and control mechanisms across different vendors.

Consequently, in real-world product environments, Chiba must be tailored to align with the specific OpenBMC firmware in use, incorporating appropriate security enhancements. This customization requirement opens up opportunities for commercialization.

## Design

Chiba is developed in Guile Scheme, a dialect of Lisp. The Scheme language is chosen for its simplicity and elegance, which makes it easy to understand and maintain. The Scheme language is also a good choice for developing a domain-specific language (DSL) for infrastructure automation.

### SCL - Server Configuration Language

Please refer to the [SCL manual](docs/scl.md) for more details.

### Firmware Security Enhancement

Please refer to the [Firmware Security Enhancement manual](docs/firmware-security-enhancement.md) for more details.

### Architecture

<center>
<img src="pub/img/chiba-arch.svg" alt="Chiba Design" width=800/>
</center>

## Installation

[English](docs/install.md) | [日本語](docs/install_jp.md)

Again, **if you are happen in Tokyo, let's have a coffee.**

## License and Authors

Chiba project is licensed under the GPLv3 license.

Chiba project is maintaining by [HardenedLinux community](https://hardenedlinux.org).
