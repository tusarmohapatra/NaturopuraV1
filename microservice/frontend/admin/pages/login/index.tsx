import React from "react";
import { BrowserWallet } from "@meshsdk/core";
import { useEffect, useState } from "react";
import { useDispatch, useSelector } from 'react-redux';
import { useRouter } from "next/router";
import { login } from '../../action/authActions';

const index = () => {

  const dispatch = useDispatch();
  const [wallets, setWallets] = useState<Array<any>>([]);

  async function connectToWallet(walletName:string) {
    const wallet = await BrowserWallet.enable(walletName);
    const addresses = await wallet.getUsedAddresses();
    const signature = await wallet.signData(addresses[0], 'mesh');
    dispatch(login({ key: signature.key, signature: signature.signature }));
  }

  const router = useRouter();
  const user = useSelector((state: any) => state.auth.user);
  const error = useSelector((state: any) => state.auth.error);


  if (typeof window !== "undefined") {
    const wallet_find = BrowserWallet.getInstalledWallets();
  }

  useEffect(() => {
    const getBrowser = BrowserWallet.getInstalledWallets();
    setWallets(getBrowser);
  }, []);

  useEffect(() => {
   
  }, []);

  useEffect(() => {
    if (user) {
      router.push("/");
    }
  }, [user]);


  useEffect(() => {
    if(error !== null && error.code == 'USER_NOT_EXIT'){
      router.push("/signup");
    }
    console.log(`user`, user);
   }, [error]);

  return (
    <div className="">
      <main className="main flex items-center flex-col">
        <img src="/images/Group-230.png" alt="" className="m-4" />
        {/* <button onClick={() => connectToWallet()}>Test</button> */}
        <label htmlFor="mySelect" className="text-red-700">Sign-up with wallet</label>
        <select
          id="mySelect"
          className="w-[10rem]"
          onChange={(event) => {
            // console.log(event.target.value, ">>>>>>");
            connectToWallet(event.target.value)
          }}
          
        >
          <option>Select a wallet</option>
          {wallets.map((item, index) => {
            return (
              <option key={index} value={item.name}>
                {item.name}
              </option>
            );
          })}
        </select>
      </main>
    </div>
  );
};

export default index;
