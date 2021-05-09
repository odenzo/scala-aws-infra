package com.odenzo.aws.ec2
import cats.effect.IO
import cats.syntax.all._
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.aws.{CIDR, OTags}
import com.odenzo.utils.IOU
import com.odenzo.utils.IOU._
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.services.ec2.model._

class VPCTest extends AWSBaseTest {

  test("Describe EIP") {
    val res = VPC.describeElasticIpNamed("k8s/dev/dial-out-01").unsafeRunSync()
    scribe.info(oprint(res))
  }

  test("Describe EIP Unattached ") {
    val prog         = VPC.describeElasticIpNamed("k8s/testinstaller/dial-out-01") >>= IOU.exactlyOne("EIP")
    val res: Address = prog.unsafeRunSync()
    scribe.info(oprint(res))
    scribe.info(s"Blah = ${res.associationId()}") // null. ,oh joy
  }

  test("Describe EIPs") {
    val res = VPC.describeElasticIP("k8s/cluster", "dev").unsafeRunSync()
    scribe.info(oprint(res))
  }

  test("Describe AggId") {
    val res = VPC.describeAggregateIds().unsafeRunSync()
    scribe.info(oprint(res))
  }

  test("Elaborate EIP") {
    val prog = for {
      eip <- VPC.describeElasticIpNamed("k8s/dev/dial-out-01") >>= exactlyOne("dial-out-01 EIP")
      _   <- IO(scribe.debug(s"Found EIP = ${oprint(eip)}"))
      _   <- IO(scribe.debug(s"Associattion Id = ${eip.associationId}"))
      eni <- VPC.describeNetworkInterfaceForAssociationId(eip.associationId())
      _   <- IO(scribe.debug(s"ENI ${oprint(eni)}"))
      ec2 <- eni.traverse(ni => EC2.getInstanceById(ni.attachment().instanceId()))
      _   <- IO(scribe.debug(s"EC2 ${oprint(ec2)}"))
    } yield ()
    prog.unsafeRunSync()
  }

  test("Allocate EIP") {
    val eipTags = OTags(Map("k8s/cluster" -> "testinstaller", "k8s/role" -> "outbound-sip", "Name" -> "k8s/testinstaller/dial-out-01"))
    val prog    = for {
      allocateRs <- VPC.allocateElasticIP(eipTags)
      _          <- IO(scribe.debug(oprint(allocateRs)))
      _          <- EC2.tagResource(allocateRs.allocationId, eipTags)
    } yield allocateRs

    prog.unsafeRunSync()
  }

  test("Look for VPC") {
    val id       = "vpc-0c75ed5483b6074f3"
    val res: Vpc = VPC.getVpcById(id).unsafeRunSync()
    scribe.info(s"VPC: ${oprint(res)}")
  }

  test("Create a VPC") {
    val cidr = CidrBlock.builder().cidrBlock("10.3.0.0/16").build()
    scribe.info(s"CIDR Block: $cidr")
    val rs   = VPC.createVpc(cidr)
    val vpc  = rs.unsafeRunSync()
    scribe.info(s"VPC ${oprint(vpc)}")
  }

  test("Post Initial VPC") {
    val vpcId          = "vpc-01d08b4b00fb7e2fd"
    val prog: IO[Unit] = for {
      vpc <- VPC.getVpcById(vpcId)
      _   <- IO(scribe.info(s"Existing VPC ${oprint(vpc)}"))
      _   <- EC2.tagResources(List(vpcId), globalTags.withTag("Name" -> "vpc-clustername"))
      _   <- VPC.enableDnsOnVpc(vpc)
      igw <- VPC.createInternetGateway(vpc) // Create IGW and Associate with VPC. Max one
      _   <- EC2.tagResources(List(igw.internetGatewayId()), globalTags.withTag("Name" -> "k8s-clustername"))

      // VPC has a NetworkACL firewall thin, untagged, Need to find it.  Inbound/Outbound/Subnets/Tags
      //     has DHCP Options we don;t care about - autodeleted but NTPServers is blank. We use those I assume?
      //     has RouteTable ... we will need to modify this I am pretty sure, add route to internet gateway
      //   eksctl made 0.0.0.0/0 goto an igw for main route table
      //   subnet route tables  for public get the same.
      //   subnet route tables for privuate subnets get a nat gateway with private IP and a public EIP
      // Create Subnet
      // Create ROutine Tableor Security Groups etc
    } yield ()

    prog.unsafeRunSync()
  }

  test("Create an EIP") {
    val eipTags                      = OTags(Map("k8s/cluster" -> "testinstaller", "Project" -> "k8s/devdebris", "Name" -> "k8s/testinstaller/dial-out-01"))
    val eip: AllocateAddressResponse = VPC.allocateElasticIP(eipTags).unsafeRunSync()
    scribe.info(s"EIP ${oprint(eip)}")
  }

  test("VPC Main RouteTable and Network ACL") {
    val vpcId                     = "vpc-01d08b4b00fb7e2fd"
    import scala.jdk.CollectionConverters._
    val res: IO[List[RouteTable]] = for {
      routeTables <- RouteTables.describeRouteTables()
      vpcMainRT   <- routeTables
                       .filter(_.associations().asScala.toList.exists(_.main == true))
                       .filter(rt => rt.vpcId().equals(vpcId))
                       .compile
                       .toList
    } yield vpcMainRT

    val v = res.unsafeRunSync()
    scribe.info(s"Res: ${oprint(v)}")

    //VPC.addRoute(v.head, igw, RequestHelpers.CIDR_ALL).unsafeRunSync()

  }

  test("Creating a Bunch of Subnets with Tags") {
    // Here we just want to create a number of public and private subnets spread across AZ zones.
    // I am going to stick with a standard template of 2 AZ, with 3 public and 3 private subnets in each zone.
    // This will give some flexibility... plan is for 1 public or private subnet for infra (e.g. Postgres/Kafka) per zone
    // Leaving 1 public and 1 private subnet per zone for cluster managered pods/nodes.
    // FOr a serious production systen multi-region or 3 AZ (for Kafka) is recommended but worry later.

    // Try out some logic since we must keep track of the number of existing subnets made
    // to specify the correct CIDR for each subzone.
    // This assumes we have a 10.x.0.0/16 VPC cidr and each subnet will be 10.x.N.0/24
    // It does really matter what N is just what subnet in what AZ zone at this point.
    // Then we can decide to config each one as public or private.
    // The approach is to target 2 public and 2 private subnets in first AZ and 1 public and 1 private in second AZ

    // Existing stuff that we have made as a cheat for testing
    // VPC with main routing table and outbound through the igw
    // Subnets will have a default association to VPC routing table unless specified otherwise (e.g. private subnets)
    val vpcId = "vpc-01d08b4b00fb7e2fd"
    // val routeTableId = "rtb-01fe775bc0e494254" // Cheating
    // val igw          = "igw-0f785c55d6d28d6f0" // Could look this up by tag really

    val primaryZone = "us-east-1a"
    //val secondaryZones = List("us-east-1b") // 0+ zones. For clusters 2 zones, 3 is overkill for our setup I thinks

    val prog = for {
      vpc           <- VPC.getVpcById(vpcId)
      _              = scribe.info(s"VPC CIDR: ${vpc.cidrBlock()}")
      cidr          <- CIDR.fromString(vpc.cidrBlock) // Assume it is a /16 and we start at x.y.1.0/24
      subnetCidr     = SecurityGroupHelpers.buildCidr(cidr.level16 + ".1.0/24")
      _              = scribe.info(s"Creating SubNet in AZ $primaryZone w/  CIDR $cidr")
      primaryPublic <- Subnets.createSubnet(vpc, primaryZone, subnetCidr, OTags.empty)
      _             <- Subnets.modifySubnetPublicIPv4(primaryPublic, autoAssignIPv4 = true)
      _             <- EC2.tagResources(List(primaryPublic.subnetId()), globalTags)

    } yield primaryPublic

    val subnet: Subnet = prog.unsafeRunSync()
    scribe.info(s"Created Subnet: ${oprint(subnet)}")

  }
}
