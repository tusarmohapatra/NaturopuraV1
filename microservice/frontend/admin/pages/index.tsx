import Head from "next/head";
import { CardanoWallet, MeshBadge } from "@meshsdk/react";
import { BrowserWallet } from "@meshsdk/core";
import { useEffect, useState } from "react";

export default function Home() {



  if (typeof window !== "undefined") {
    const wallet_find = BrowserWallet.getInstalledWallets();

    useEffect(() => {
      console.log(`wallet_find`, wallet_find);
    }, []);
  }

  async function connectToWallet() {
    const wallet = await BrowserWallet.enable("Nami");
    const addresses = await wallet.getUsedAddresses();
    const signature = await wallet.signData(addresses[0], 'mesh');
    console.log(`wallet`,wallet,signature ,addresses);
  }
  


  return (
    <div className="container">
      <main className="main">
        <h1 className="title">
          <a href="https://meshjs.dev/">Mesh</a> Next.js
        </h1>

        <button onClick={()=>connectToWallet()}>Test</button>
        <div className="demo">
          <CardanoWallet />
        </div>
      </main>

      <footer className="footer">
        <MeshBadge dark={true} />
      </footer>
    </div>
  );
}
